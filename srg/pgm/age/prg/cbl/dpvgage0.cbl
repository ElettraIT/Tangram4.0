       Identification Division.
       Program-Id.                                 dpvgage0           .
      *================================================================*
      *                                                                *
      * Modulo per la determinazione provvigioni agenti                *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *                                                                *
      *        Input  : d-pvg-age-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-pvg-age-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-pvg-age-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-pvg-age-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "PP" - Determinazione percentuali provvigioni                  *
      *                                                                *
      *                                                                *
      *        Input  : d-pvg-age-tip-ope = "PP"                       *
      *                                                                *
      *                 d-pvg-age-cod-age = Codice agente              *
      *                                                                *
      *                 d-pvg-age-tip-vpa = Tipo vendita per l'agente  *
      *                                     - 01 : Diretta             *
      *                                     - 02 : Indiretta           *
      *                                                                *
      *                 d-pvg-age-cpv-aaa = Categoria provvigioni as-  *
      *                                     sociata all'agente         *
      *                                                                *
      *                 d-pvg-age-ppv-aaa = 3 percentuali di provvi-   *
      *                                     gione associate all'agente *
      *                                                                *
      *                 d-pvg-age-tip-mag = Tipo codice magazzino      *
      *                                                                *
      *                 d-pvg-age-num-mag = Codice magazzino numerico  *
      *                                                                *
      *                 d-pvg-age-cod-lst = Codice listino             *
      *                                                                *
      *                 d-pvg-age-cpv-aap = Categoria provvigioni as-  *
      *                                     sociata al prodotto        *
      *                                                                *
      *                 d-pvg-age-ppv-aap = 3 percentuali di provvi-   *
      *                                     gione associate al prodot- *
      *                                     to                         *
      *                                                                *
      *                 d-pvg-age-cod-cli = Codice cliente             *
      *                                                                *
      *                 d-pvg-age-cpv-aac = Categoria provvigioni as-  *
      *                                     sociata al cliente         *
      *                                                                *
      *                 d-pvg-age-ppv-aac = 3 percentuali di provvi-   *
      *                                     gione associate al cliente *
      *                                                                *
      *                 d-pvg-age-sgl-vpp = Sigla valuta               *
      *                                                                *
      *                 d-pvg-age-prz-lrs = Prezzo lordo standard      *
      *                                                                *
      *                 d-pvg-age-prz-nts = Prezzo netto standard      *
      *                                                                *
      *                 d-pvg-age-prz-net = Prezzo netto effettivo     *
      *                                                                *
      *                 d-pvg-age-per-scr = 5 percentuali di sconto in *
      *                                     riga                       *
      *                                                                *
      *                 d-pvg-age-dat-rif = Data riferimento per de-   *
      *                                     terminazione provvigioni   *
      *                                                                *
      *                                                                *
      *        Output : d-pvg-age-per-pvg = 3 percentuali provvigioni  *
      *                                                                *
      *                 d-pvg-age-flg-pvs = Flag di provvigioni spe-   *
      *                                     ciali                      *
      *                                      - Spaces : No             *
      *                                      - #      : Speciali       *
      *                                                                *
      * -------------------------------------------------------------- *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zpv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfzpv"                          .
      *        *-------------------------------------------------------*
      *        * [zpx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfzpx"                          .
      *        *-------------------------------------------------------*
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [lsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflsd"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Determinazione % di provvigione in ordini-bolle-fat-  *
      *        * ture                                                  *
      *        *-------------------------------------------------------*
           05  w-prs-dpp-obf.
      *            *---------------------------------------------------*
      *            * Numero elementi in tabella                        *
      *            *---------------------------------------------------*
               10  w-prs-dpp-obf-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero massimo elementi in tabella                *
      *            *---------------------------------------------------*
               10  w-prs-dpp-obf-max      pic  9(03) value 15         .
      *            *---------------------------------------------------*
      *            * Tabella tipi di calcolo                           *
      *            *---------------------------------------------------*
               10  w-prs-dpp-obf-tbl occurs 15.
      *                *-----------------------------------------------*
      *                * Codice di calcolo                             *
      *                *-----------------------------------------------*
                   15  w-prs-dpp-obf-cdc  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Segnale di continuazione con calcolo succes-  *
      *                * sivo                                          *
      *                *-----------------------------------------------*
                   15  w-prs-dpp-obf-sdc  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work-area di ridefinizione personalizzazione      *
      *            *---------------------------------------------------*
               10  w-prs-dpp-obf-wst.
      *                *-----------------------------------------------*
      *                * 14 elementi per il calcolo                    *
      *                *-----------------------------------------------*
                   15  w-prs-dpp-obf-wel occurs 15.
                       20  w-prs-dpp-obf-aaa
                                          pic  9(02)                  .
                       20  w-prs-dpp-obf-bbb
                                          pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Area libera per utilizzi futuri               *
      *                *-----------------------------------------------*
                   15  w-prs-dpp-obf-wal  pic  x(06)                  .
      *                *-----------------------------------------------*
      *                * Metodo per la determinazione della % di scon- *
      *                * to                                            *
      *                *-----------------------------------------------*
                   15  w-prs-dpp-obf-wmd  pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Work-area di comodo                               *
      *            *---------------------------------------------------*
               10  w-prs-dpp-obf-wcc      pic  9(02)                  .
               10  w-prs-dpp-obf-wsc      pic  x(01)                  .
               10  w-prs-dpp-obf-c01      pic  9(03)                  .
               10  w-prs-dpp-obf-c02      pic  9(03)                  .
       01  w-prs-abr.
      *        *-------------------------------------------------------*
      *        * Si/No abbattimento in riga                            *
      *        *                                                       *
      *        * - NO : No                                             *
      *        * - SI : Si, a meno che l'ammontare provvigione non     *
      *        *        diventi negativo                               *
      *        * - RI : Come il precedente ma il calcolo avviene in    *
      *        *        fase di inserimento delle righe                *
      *        * - S- : Si, anche in caso di ammontare della provvi-   *
      *        *        gione negativo                                 *
      *        * - Fx : Si, ma solo se il flag numero x (1..9) in ana- *
      *        *            anagrafica agenti e' posto al valore 'X'   *
      *        *-------------------------------------------------------*
           05  w-prs-abr-snx-abr          pic  x(02)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Metodo di determinazione della percentuale di sconto  *
      *        * effettiva                                             *
      *        *                                                       *
      *        * - 01 : La % di sconto applicata deve essere desunta   *
      *        *        dalla proporzione tra prezzo lordo standard    *
      *        *        e prezzo netto effettivamente praticato        *
      *        *        zo netto effettivamente espresso in fattura    *
      *        *                                                       *
      *        * - 02 : La % di sconto applicata deve essere desunta   *
      *        *        dalla proporzione tra prezzo netto standard    *
      *        *        e prezzo netto effettivamente praticato        *
      *        *                                                       *
      *        * - 03 : La % di sconto applicata deve essere desunta   *
      *        *        dalla sommatoria scalare dei max 5 sconti      *
      *        *        effettivamente praticati, senza tener conto    *
      *        *        del prezzo                                     *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-prs-abr-det-pse          pic  9(02)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Modalita' di abbattimento se sconto                   *
      *        *                                                       *
      *        * - 000 : No abbattimento                               *
      *        * - 100 : Alla percentuale di provvigione normale viene *
      *        *         sottratta la percentuale di sconto effettiva  *
      *        * - 1nn : Alla percentuale di provvigione normale viene *
      *        *         sottratto l'nn % della percentuale di sconto  *
      *        *         effettiva                                     *
      *        *                                                       *
      *        *         N.B.: Se il valore del campo 'scd' esiste,    *
      *        *               questo viene utilizzato come decimale   *
      *        *               per la % di abbattimento                *
      *        *-------------------------------------------------------*
           05  w-prs-abr-mab-sco          pic  9(03)                  .
           05  w-prs-abr-mab-scd          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Modalita' di aumento se maggiorazione                 *
      *        *                                                       *
      *        * - 000 : No aumento                                    *
      *        *                                                       *
      *        * - 100 : Alla percentuale di provvigione normale viene *
      *        *         aggiunta la percentuale di maggiorazione ef-  *
      *        *         fettiva                                       *
      *        *                                                       *
      *        * - 1nn : Alla percentuale di provvigione normale viene *
      *        *         aggiunto l'nn % della percentuale di maggio-  *
      *        *         razione effettiva                             *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-prs-abr-mau-mag          pic  9(03)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Percentuale di provvigione massima ammessa, conside-  *
      *        * rata solo dopo l'applicazione di un aumento           *
      *        *-------------------------------------------------------*
           05  w-prs-abr-max-psa          pic  9(02)                  .
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Percentuale di sconto massima ammessa, al di sotto    *
      *        * della quale non viene applicato l'eventuale abbat-    *
      *        * timento in riga                                       *
      *        *-------------------------------------------------------*
           05  w-prs-abr-max-psc          pic  9(02)                  .
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

      *    *===========================================================*
      *    * Work area per determinazioni varie                        *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work area di appoggio alla routine di determinazione  *
      *        *-------------------------------------------------------*
           05  w-det-age.
      *            *---------------------------------------------------*
      *            * Comodo per indice su ultima provvigione signifi-  *
      *            * cativa determinata                                *
      *            *---------------------------------------------------*
               10  w-det-age-iup-pvg      pic   9(02)                 .
      *            *---------------------------------------------------*
      *            * Comodo per codice di calcolo in esame             *
      *            *---------------------------------------------------*
               10  w-det-age-cdc-pvg      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodo per segnale di continuazione con il calco- *
      *            * lo successivo in esame                            *
      *            *---------------------------------------------------*
               10  w-det-age-scd-pvg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di determinazione calcolo effettuata         *
      *            * - Spaces : Non effettuata                         *
      *            * - #      : Effettuata                             *
      *            *---------------------------------------------------*
               10  w-det-age-fde-pvg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di significativita' determinazione effettua- *
      *            * ta                                                *
      *            * - Spaces : Non significativo                      *
      *            * - #      : Significativo                          *
      *            *---------------------------------------------------*
               10  w-det-age-fsd-pvg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Comodo per % di provvigione per aggiornamento     *
      *            *---------------------------------------------------*
               10  w-det-age-agg-pvg occurs 03
                                          pic  9(02)v9(01)            .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-age-ctr-001      pic  9(02)                  .
               10  w-det-age-ctr-002      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Elementi di comodo                                *
      *            *---------------------------------------------------*
               10  w-det-age-cat-aap      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Per determinazione della voce 'percentuale di sconto' *
      *        *-------------------------------------------------------*
           05  w-det-per-sco.
      *            *---------------------------------------------------*
      *            * Valore determinato                                *
      *            *---------------------------------------------------*
               10  w-det-per-sco-per      pic   9(02)v9(01)           .
      *        *-------------------------------------------------------*
      *        * Per determinazione % unica da max 5 %                 *
      *        *-------------------------------------------------------*
           05  w-det-per-uni.
      *            *---------------------------------------------------*
      *            * Le cinque %                                       *
      *            *---------------------------------------------------*
               10  w-det-per-uni-001      pic   9(02)v9(01)           .
               10  w-det-per-uni-002      pic   9(02)v9(01)           .
               10  w-det-per-uni-003      pic   9(02)v9(01)           .
               10  w-det-per-uni-004      pic   9(02)v9(01)           .
               10  w-det-per-uni-005      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * La % unica corrispondente                         *
      *            *---------------------------------------------------*
               10  w-det-per-uni-per      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * Work locale                                       *
      *            *---------------------------------------------------*
               10  w-det-per-uni-w01      pic   9(07)v9(01)           .
               10  w-det-per-uni-w02      pic   9(07)v9(01)           .
               10  w-det-per-uni-w03      pic   9(07)v9(01)           .
      *        *-------------------------------------------------------*
      *        * Per determinazione % sconto in base a due prezzi      *
      *        *-------------------------------------------------------*
           05  w-det-pes-prz.
      *            *---------------------------------------------------*
      *            * 1. prezzo in input                                *
      *            *---------------------------------------------------*
               10  w-det-pes-prz-pi1      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * 2. prezzo in input                                *
      *            *---------------------------------------------------*
               10  w-det-pes-prz-pi2      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Comodo di transito                                *
      *            *---------------------------------------------------*
               10  w-det-pes-prz-s13      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Comodo di transito                                *
      *            *---------------------------------------------------*
               10  w-det-pes-prz-s11      pic s9(11)v9(01)            .
      *            *---------------------------------------------------*
      *            * % di sconto in output                             *
      *            *---------------------------------------------------*
               10  w-det-pes-prz-sco      pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Per determinazione % di provvigioni in base agli      *
      *        * scaglioni di sconti in tabella [zpv]                  *
      *        *-------------------------------------------------------*
           05  w-det-pps-zpv.
      *            *---------------------------------------------------*
      *            * % di sconto unica calcolata                       *
      *            *---------------------------------------------------*
               10  w-det-pps-zpv-psc      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * Tipo vendita                                      *
      *            *---------------------------------------------------*
               10  w-det-pps-zpv-vpa      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * Indice di comodo                                  *
      *            *---------------------------------------------------*
               10  w-det-pps-zpv-inx      pic   9(02)                 .
      *            *---------------------------------------------------*
      *            * Indice elemento selezionato                       *
      *            *---------------------------------------------------*
               10  w-det-pps-zpv-ies      pic   9(02)                 .
      *            *---------------------------------------------------*
      *            * % di provvigione rilevata                         *
      *            *---------------------------------------------------*
               10  w-det-pps-zpv-per      pic   9(02)v9(01)           .
      *        *-------------------------------------------------------*
      *        * Per determinazione % di provvigioni in base agli      *
      *        * scaglioni di sconti in tabella [zpx]                  *
      *        *-------------------------------------------------------*
           05  w-det-pps-zpx.
      *            *---------------------------------------------------*
      *            * % di sconto unica calcolata                       *
      *            *---------------------------------------------------*
               10  w-det-pps-zpx-psc      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * Tipo vendita                                      *
      *            *---------------------------------------------------*
               10  w-det-pps-zpx-vpa      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * Indice di comodo                                  *
      *            *---------------------------------------------------*
               10  w-det-pps-zpx-inx      pic   9(02)                 .
      *            *---------------------------------------------------*
      *            * Indice elemento selezionato                       *
      *            *---------------------------------------------------*
               10  w-det-pps-zpx-ies      pic   9(02)                 .
      *            *---------------------------------------------------*
      *            * % di provvigione rilevata                         *
      *            *---------------------------------------------------*
               10  w-det-pps-zpx-per      pic   9(02)v9(01)           .
      *        *-------------------------------------------------------*
      *        * Per determinazione provvigioni da singola riga        *
      *        *-------------------------------------------------------*
           05  w-det-psr.
      *            *---------------------------------------------------*
      *            * Flag di determinazione eseguita oppure no         *
      *            *  - S : Si                                         *
      *            *  - N : No                                         *
      *            *---------------------------------------------------*
               10  w-det-psr-snx-det      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Imponibile provvigionale da singola riga          *
      *            *---------------------------------------------------*
               10  w-det-psr-ibl-pvg      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Importo provvigioni da singola riga               *
      *            *---------------------------------------------------*
               10  w-det-psr-amm-pvg      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * % di provvigione da singola riga                  *
      *            *---------------------------------------------------*
               10  w-det-psr-per-pvg      pic  9(02)v9(01)            .
      *            *---------------------------------------------------*
      *            * % di sconto che ha determinato un abbattimento in *
      *            * riga                                              *
      *            *---------------------------------------------------*
               10  w-det-psr-pds-abr      pic  9(03)v9(01)            .
      *            *---------------------------------------------------*
      *            * % di maggiorazione che ha determinato un abbatti- *
      *            * mento in riga                                     *
      *            *---------------------------------------------------*
               10  w-det-psr-pdm-abr      pic  9(03)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Per determinazione abbattimento/aumento in riga       *
      *        *-------------------------------------------------------*
           05  w-det-abt-rig.
      *            *---------------------------------------------------*
      *            * Percentuale unica di sconto determinata           *
      *            *---------------------------------------------------*
               10  w-det-abt-rig-pus      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * Percentuale unica di maggiorazione determinata    *
      *            *---------------------------------------------------*
               10  w-det-abt-rig-pum      pic   9(07)v9(01)           .
      *            *---------------------------------------------------*
      *            * Differenza prezzo tra i prezzi da confrontare per *
      *            * dedurre la percentuale unica di sconto o di mag-  *
      *            * giorazione                                        *
      *            *---------------------------------------------------*
               10  w-det-abt-rig-dpf      pic  s9(09)                 .
      *            *---------------------------------------------------*
      *            * Percentuale di abbattimento/maggiorazione, solo   *
      *            * due interi ed un decimale facoltativo             *
      *            *---------------------------------------------------*
               10  w-det-abt-rig-pda      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * Percentuale di comodo                             *
      *            *---------------------------------------------------*
               10  w-det-abt-rig-pwa      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * Percentuale di sconto/maggiorazione abbattuta     *
      *            *---------------------------------------------------*
               10  w-det-abt-rig-psa      pic   9(07)v9(01)           .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione provvigioni      *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/dpvgage0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-pvg-age              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-pvg-age-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        d-pvg-age-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-pvg-age-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-pvg-age-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Determinazione provvigioni                  *
      *                  *---------------------------------------------*
           else if   d-pvg-age-tip-ope    =    "PP"
                     perform dpp-000      thru dpp-999                .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
       opn-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione % di provvigione in ordini-  *
      *                  * bolle-fatture                               *
      *                  *---------------------------------------------*
           perform   prs-dpp-obf-000      thru prs-dpp-obf-999        .
      *                  *---------------------------------------------*
      *                  * Abbattimenti provvigionali in riga          *
      *                  *---------------------------------------------*
           perform   prs-age-abr-000      thru prs-age-abr-999        .
       opn-200.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [zpv]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * [zpx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpx                 .
      *                  *---------------------------------------------*
      *                  * [lst]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * [lsd]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-nol-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [zpv]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * [zpx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofzpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpx                 .
      *                  *---------------------------------------------*
      *                  * [lst]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * [lsd]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-nol-ctr-opn        =    zero
                     move  spaces         to   d-pvg-age-exi-sts
           else      move  "#"            to   d-pvg-age-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di provvigione                 *
      *    *-----------------------------------------------------------*
       dpp-000.
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
      *              * Normalizzazione valori in output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-pvg-age-per-pvg (1)  .
           move      zero                 to   d-pvg-age-per-pvg (2)  .
           move      zero                 to   d-pvg-age-per-pvg (3)  .
           move      spaces               to   d-pvg-age-flg-pvs      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di comodo                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice ultima provvigione si-  *
      *              * gnificativa                                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-age-iup-pvg      .
       dpp-020.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se personalizzazione su tipo calcolo indica *
      *                  * 'Nessun calcolo' : attivazione flag indi-   *
      *                  * cante provvigioni speciali ed a controllo   *
      *                  * finale                                      *
      *                  *---------------------------------------------*
           if        w-prs-dpp-obf-cdc (1)
                                          =    00 and
                     w-prs-dpp-obf-sdc (1)
                                          =    " "
                     move  "#"            to   d-pvg-age-flg-pvs
                     go to dpp-980.
       dpp-030.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-age-ctr-001      .
       dpp-050.
           add       1                    to   w-det-age-ctr-001      .
           if        w-det-age-ctr-001    >    w-prs-dpp-obf-ele
                     go to dpp-980.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di determinazione effet-  *
      *              * tuata a : No                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-age-fde-pvg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di significativita' de-   *
      *              * terminazione effettuata a : No                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-age-fsd-pvg      .
      *              *-------------------------------------------------*
      *              * Codice di calcolo in work di comodo             *
      *              *-------------------------------------------------*
           move      w-prs-dpp-obf-cdc
                    (w-det-age-ctr-001)   to   w-det-age-cdc-pvg      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del codice di calcolo    *
      *              *-------------------------------------------------*
           if        w-det-age-cdc-pvg    =    00
                     go to dpp-100
           else if   w-det-age-cdc-pvg    =    11
                     go to dpp-110
           else if   w-det-age-cdc-pvg    =    12
                     go to dpp-120
           else if   w-det-age-cdc-pvg    =    21
                     go to dpp-210
           else if   w-det-age-cdc-pvg    =    22
                     go to dpp-220
           else if   w-det-age-cdc-pvg    =    31
                     go to dpp-310
           else if   w-det-age-cdc-pvg    =    32
                     go to dpp-320
           else if   w-det-age-cdc-pvg    =    41
                     go to dpp-410
           else if   w-det-age-cdc-pvg    =    51
                     go to dpp-510
           else if   w-det-age-cdc-pvg    =    52
                     go to dpp-520
           else if   w-det-age-cdc-pvg    =    61
                     go to dpp-610
           else if   w-det-age-cdc-pvg    =    71
                     go to dpp-710
           else if   w-det-age-cdc-pvg    =    72
                     go to dpp-720
           else if   w-det-age-cdc-pvg    =    73
                     go to dpp-730
           else if   w-det-age-cdc-pvg    =    74
                     go to dpp-740.
       dpp-100.
      *              *=================================================*
      *              * Codice di calcolo : Nessun calcolo              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-110.
      *              *=================================================*
      *              * Codice di calcolo : % dirette da anagrafica a-  *
      *              * gente                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice agente a zero : A controllo fina- *
      *                  * le                                          *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-age    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-ppv-aaa (1)
                                          =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      d-pvg-age-ppv-aaa (1)
                                          to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-120.
      *              *=================================================*
      *              * Codice di calcolo : % indirette da categoria    *
      *              * provvigionale in anagrafica agente              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice agente a zero : A controllo fina- *
      *                  * le                                          *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-age    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aaa   =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      01                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aaa    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Determinazione della voce '% di sconto'     *
      *                  *---------------------------------------------*
           perform   det-per-sco-000      thru det-per-sco-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione % provvigioni in tabella     *
      *                  * [zpv]                                       *
      *                  *---------------------------------------------*
           move      w-det-per-sco-per    to   w-det-pps-zpv-psc      .
           move      d-pvg-age-tip-vpa    to   w-det-pps-zpv-vpa      .
           perform   det-pps-zpv-000      thru det-pps-zpv-999        .
       dpp-150.
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        w-det-pps-zpv-per    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      w-det-pps-zpv-per    to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-210.
      *              *=================================================*
      *              * Codice di calcolo : % dirette da anagrafica     *
      *              * prodotto                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo codice di magazzino diverso da pro- *
      *                  * dotto di vendita : A controllo finale       *
      *                  *---------------------------------------------*
           if        d-pvg-age-tip-mag    not  = 01
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se codice numerico di magazzino a zero : A  *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-num-mag    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-ppv-aap (1)
                                          =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      d-pvg-age-ppv-aap (1)
                                          to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-220.
      *              *=================================================*
      *              * Codice di calcolo : % indirette da categoria    *
      *              * provvigionale in anagrafica prodotto            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo codice di magazzino diverso da pro- *
      *                  * dotto di vendita : A controllo finale       *
      *                  *---------------------------------------------*
           if        d-pvg-age-tip-mag    not  = 01
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se codice numerico di magazzino a zero : A  *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-num-mag    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aap     =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      02                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aap    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Determinazione della voce '% di sconto'     *
      *                  *---------------------------------------------*
           perform   det-per-sco-000      thru det-per-sco-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione % provvigioni in tabella     *
      *                  * [zpv]                                       *
      *                  *---------------------------------------------*
           move      w-det-per-sco-per    to   w-det-pps-zpv-psc      .
           move      d-pvg-age-tip-vpa    to   w-det-pps-zpv-vpa      .
           perform   det-pps-zpv-000      thru det-pps-zpv-999        .
       dpp-250.
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        w-det-pps-zpv-per    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      w-det-pps-zpv-per    to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-310.
      *              *=================================================*
      *              * Codice di calcolo : % dirette da anagrafica     *
      *              * commerciale cliente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice cliente a zero : A controllo fi-  *
      *                  * nale                                        *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-cli    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-ppv-aac (1)
                                          =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      d-pvg-age-ppv-aac (1)
                                          to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-320.
      *              *=================================================*
      *              * Codice di calcolo : % indirette da categoria    *
      *              * provvigionale in anagrafica commerciale cliente *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice cliente a zero : A controllo fi-  *
      *                  * nale                                        *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-cli    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aac    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      03                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aac    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Determinazione della voce '% di sconto'     *
      *                  *---------------------------------------------*
           perform   det-per-sco-000      thru det-per-sco-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione % provvigioni in tabella     *
      *                  * [zpv]                                       *
      *                  *---------------------------------------------*
           move      w-det-per-sco-per    to   w-det-pps-zpv-psc      .
           move      d-pvg-age-tip-vpa    to   w-det-pps-zpv-vpa      .
           perform   det-pps-zpv-000      thru det-pps-zpv-999        .
       dpp-350.
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        w-det-pps-zpv-per    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      w-det-pps-zpv-per    to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-410.
      *              *=================================================*
      *              * Codice di calcolo : % dirette contenute nei     *
      *              * prezzi netti concordati per cliente             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice cliente a zero : A controllo fi-  *
      *                  * nale                                        *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-cli    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se tipo codice di magazzino diverso da pro- *
      *                  * dotto di vendita : A controllo finale       *
      *                  *---------------------------------------------*
           if        d-pvg-age-tip-mag    not  = 01
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se codice numerico di magazzino a zero : A  *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-num-mag    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [lst]                      *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      02                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      d-pvg-age-cod-cli    to   rf-lst-cod-cli         .
           move      d-pvg-age-sgl-vpp    to   rf-lst-sgl-vlt         .
           move      d-pvg-age-num-mag    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se Si/no utilizzo delle % di provvigione    *
      *                  * nella determinazione delle provvigioni con- *
      *                  * tenuto nel record a No : A controllo finale *
      *                  *---------------------------------------------*
           if        rf-lst-snx-pvg       not  = "S"
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Test su data di validita' iniziale          *
      *                  *---------------------------------------------*
           if        rf-lst-dva-ini       =    zero
                     go to dpp-420.
           if        rf-lst-dva-ini       >    d-pvg-age-dat-rif
                     go to dpp-800.
       dpp-420.
      *                  *---------------------------------------------*
      *                  * Test su data di validita' finale            *
      *                  *---------------------------------------------*
           if        rf-lst-dva-fin       =    zero
                     go to dpp-430.
           if        rf-lst-dva-fin       <    d-pvg-age-dat-rif
                     go to dpp-800.
       dpp-430.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Attivazione flag indicante provvigioni spe- *
      *                  * ciali                                       *
      *                  *---------------------------------------------*
           move      "#"                  to   d-pvg-age-flg-pvs      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      rf-lst-per-pvg (1)   to   w-det-age-agg-pvg (1)  .
           move      rf-lst-per-pvg (2)   to   w-det-age-agg-pvg (2)  .
           move      rf-lst-per-pvg (3)   to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-510.
      *              *=================================================*
      *              * Codice di calcolo : % dirette contenute nei     *
      *              * prezzi per campagna di vendita                  *
      *              *                                                 *
      *              * Tipo calcolo : 51                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo codice di magazzino diverso da pro- *
      *                  * dotto di vendita : A controllo finale       *
      *                  *---------------------------------------------*
           if        d-pvg-age-tip-mag    not  = 01
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se codice numerico di magazzino a zero : A  *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-num-mag    =    zero
                     go to dpp-800.
       dpp-512.
      *                  *---------------------------------------------*
      *                  * Determinazione provvigioni per campagna di  *
      *                  * vendita attuale                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [lst]                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      03                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      c-sgl                to   rf-lst-sgl-vlt         .
           move      d-pvg-age-num-mag    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : oltre               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-515.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lst-dva-ini       >    d-pvg-age-dat-rif
                     go to dpp-515.
      *                      *-----------------------------------------*
      *                      * Test su data di fine campagna           *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       <    d-pvg-age-dat-rif
                     go to dpp-515.
      *                      *-----------------------------------------*
      *                      * Se Si/no utilizzo delle % di provvigio- *
      *                      * ne nella determinazione delle provvi-   *
      *                      * gioni contenuto nel record a No : oltre *
      *                      *-----------------------------------------*
           if        rf-lst-snx-pvg       not  = "S"
                     go to dpp-515.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata a: Si *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                      *-----------------------------------------*
      *                      * Attivazione flag indicante provvigioni  *
      *                      * speciali                                *
      *                      *-----------------------------------------*
           move      "#"                  to   d-pvg-age-flg-pvs      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di provvigio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           move      rf-lst-per-pvg (1)   to   w-det-age-agg-pvg (1)  .
           move      rf-lst-per-pvg (2)   to   w-det-age-agg-pvg (2)  .
           move      rf-lst-per-pvg (3)   to   w-det-age-agg-pvg (3)  .
      *                      *-----------------------------------------*
      *                      * Sub-routine di aggiornamento            *
      *                      *-----------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di provvigione tutte si- *
      *                      * gnificative : uscita immediata          *
      *                      *-----------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpp-800.
       dpp-515.
      *                  *---------------------------------------------*
      *                  * Determinazione provvigioni per campagna di  *
      *                  * vendita storicizzata                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      03                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      c-sgl                to   rf-lsd-sgl-vlt         .
           move      d-pvg-age-num-mag    to   rf-lsd-num-pro         .
           move      d-pvg-age-dat-rif    to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 03                or
                     rf-lsd-cod-lst       not  = spaces            or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = c-sgl             or
                     rf-lsd-num-pro       not  = d-pvg-age-num-mag
                     go to dpp-800.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-ini       >    d-pvg-age-dat-rif
                     go to dpp-800.
      *                      *-----------------------------------------*
      *                      * Se Si/no utilizzo delle % di provvigio- *
      *                      * ne nella determinazione delle provvi-   *
      *                      * gioni contenuto nel record a No : A     *
      *                      * controllo finale                        *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-pvg       not  = "S"
                     go to dpp-800.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata a: Si *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                      *-----------------------------------------*
      *                      * Attivazione flag indicante provvigioni  *
      *                      * speciali                                *
      *                      *-----------------------------------------*
           move      "#"                  to   d-pvg-age-flg-pvs      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di provvigio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           move      rf-lsd-per-pvg (1)   to   w-det-age-agg-pvg (1)  .
           move      rf-lsd-per-pvg (2)   to   w-det-age-agg-pvg (2)  .
           move      rf-lsd-per-pvg (3)   to   w-det-age-agg-pvg (3)  .
      *                      *-----------------------------------------*
      *                      * Sub-routine di aggiornamento            *
      *                      *-----------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di provvigione tutte si- *
      *                      * gnificative : uscita immediata          *
      *                      *-----------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpp-800.
       dpp-520.
      *              *=================================================*
      *              * Codice di calcolo : % dirette contenute nei     *
      *              * prezzi per campagna di vendita                  *
      *              *                                                 *
      *              * Tipo calcolo : 52                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sub-routine                                 *
      *                  *---------------------------------------------*
           perform   dpp-052-000          thru dpp-052-999            .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-610.
      *              *=================================================*
      *              * Codice di calcolo : % dirette contenute nei     *
      *              * prezzi di listino                               *
      *              *                                                 *
      *              * Tipo calcolo : 61                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo codice di magazzino diverso da pro- *
      *                  * dotto di vendita : A controllo finale       *
      *                  *---------------------------------------------*
           if        d-pvg-age-tip-mag    not  = 01
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se codice numerico di magazzino a zero : A  *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-num-mag    =    zero
                     go to dpp-800.
       dpp-612.
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di listino storico    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      01                   to   rf-lsd-tip-rec         .
           move      d-pvg-age-cod-lst    to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      d-pvg-age-sgl-vpp    to   rf-lsd-sgl-vlt         .
           move      d-pvg-age-num-mag    to   rf-lsd-num-pro         .
           move      d-pvg-age-dat-rif    to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-613.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-613.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 01                or
                     rf-lsd-cod-lst       not  = d-pvg-age-cod-lst or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = d-pvg-age-sgl-vpp or
                     rf-lsd-num-pro       not  = d-pvg-age-num-mag
                     go to dpp-613.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata a: Si *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                      *-----------------------------------------*
      *                      * Attivazione flag indicante provvigioni  *
      *                      * speciali                                *
      *                      *-----------------------------------------*
           move      "#"                  to   d-pvg-age-flg-pvs      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di provvigio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           move      rf-lsd-per-pvg (1)   to   w-det-age-agg-pvg (1)  .
           move      rf-lsd-per-pvg (2)   to   w-det-age-agg-pvg (2)  .
           move      rf-lsd-per-pvg (3)   to   w-det-age-agg-pvg (3)  .
      *                      *-----------------------------------------*
      *                      * Sub-routine di aggiornamento            *
      *                      *-----------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di provvigione tutte si- *
      *                      * gnificative : uscita immediata          *
      *                      *-----------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpp-800.
       dpp-613.
      *                  *---------------------------------------------*
      *                  * Se codice listino a spaces : A determina-   *
      *                  * zione valori da anagrafica [dcp] come per   *
      *                  * codice di calcolo 21                        *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-lst    =    spaces
                     go to dpp-210.
       dpp-614.
      *                  *---------------------------------------------*
      *                  * Determinazione provvigioni da listino di    *
      *                  * vendita attuale                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [lst]                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      01                   to   rf-lst-tip-rec         .
           move      d-pvg-age-cod-lst    to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      d-pvg-age-sgl-vpp    to   rf-lst-sgl-vlt         .
           move      d-pvg-age-num-mag    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : oltre               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata a: Si *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento percentuali di provvigio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           move      rf-lst-per-pvg (1)   to   w-det-age-agg-pvg (1)  .
           move      rf-lst-per-pvg (2)   to   w-det-age-agg-pvg (2)  .
           move      rf-lst-per-pvg (3)   to   w-det-age-agg-pvg (3)  .
      *                      *-----------------------------------------*
      *                      * Sub-routine di aggiornamento            *
      *                      *-----------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                      *-----------------------------------------*
      *                      * Se percentuali di provvigione tutte si- *
      *                      * gnificative : uscita immediata          *
      *                      *-----------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpp-800.
       dpp-710.
      *              *=================================================*
      *              * Codice di calcolo : % indirette per incrocio    *
      *              * tra :                                           *
      *              *                                                 *
      *              * - categoria provvigionale contenuta in anagra-  *
      *              *   fica agente                                   *
      *              * - categoria provvigionale contenuta in anagra-  *
      *              *   fica prodotto                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice agente a zero : A controllo fina- *
      *                  * le                                          *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-age    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aaa    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      01                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aaa    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se tipo codice di magazzino diverso da pro- *
      *                  * dotto di vendita : A controllo finale       *
      *                  *---------------------------------------------*
           if        d-pvg-age-tip-mag    not  = 01
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se codice numerico di magazzino a zero : A  *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-num-mag    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aap    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      02                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aap    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpx]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "UNODUE"             to   f-key                  .
           move      01                   to   rf-zpx-tip-inc         .
           move      d-pvg-age-cpv-aaa    to   rf-zpx-cod-uno         .
           move      d-pvg-age-cpv-aap    to   rf-zpx-cod-due         .
           move      "pgm/age/fls/ioc/obj/iofzpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpx                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Determinazione della voce '% di sconto'     *
      *                  *---------------------------------------------*
           perform   det-per-sco-000      thru det-per-sco-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione % provvigioni in tabella     *
      *                  * [zpx]                                       *
      *                  *---------------------------------------------*
           move      w-det-per-sco-per    to   w-det-pps-zpx-psc      .
           move      d-pvg-age-tip-vpa    to   w-det-pps-zpx-vpa      .
           perform   det-pps-zpx-000      thru det-pps-zpx-999        .
       dpp-715.
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        w-det-pps-zpx-per    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      w-det-pps-zpx-per    to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-720.
      *              *=================================================*
      *              * Codice di calcolo : % indirette per incrocio    *
      *              * tra :                                           *
      *              * - categoria provvigionale contenuta in anagra-  *
      *              *   fica agente                                   *
      *              * - categoria provvigionale contenuta in anagra-  *
      *              *   fica commerciale cliente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice agente a zero : A controllo fina- *
      *                  * le                                          *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-age    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aaa    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      01                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aaa    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se codice cliente a zero : A controllo fi-  *
      *                  * nale                                        *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-cli    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aac    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      03                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aac    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpx]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "UNODUE"             to   f-key                  .
           move      02                   to   rf-zpx-tip-inc         .
           move      d-pvg-age-cpv-aaa    to   rf-zpx-cod-uno         .
           move      d-pvg-age-cpv-aac    to   rf-zpx-cod-due         .
           move      "pgm/age/fls/ioc/obj/iofzpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpx                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Determinazione della voce '% di sconto'     *
      *                  *---------------------------------------------*
           perform   det-per-sco-000      thru det-per-sco-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione % provvigioni in tabella     *
      *                  * [zpx]                                       *
      *                  *---------------------------------------------*
           move      w-det-per-sco-per    to   w-det-pps-zpx-psc      .
           move      d-pvg-age-tip-vpa    to   w-det-pps-zpx-vpa      .
           perform   det-pps-zpx-000      thru det-pps-zpx-999        .
       dpp-725.
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        w-det-pps-zpx-per    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      w-det-pps-zpx-per    to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-730.
      *              *=================================================*
      *              * Codice di calcolo : % indirette per incrocio    *
      *              * tra :                                           *
      *              * - categoria provvigionale contenuta in anagra-  *
      *              *   fica commerciale cliente                      *
      *              * - categoria provvigionale contenuta in anagra-  *
      *              *   fica prodotto                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice cliente a zero : A controllo fi-  *
      *                  * nale                                        *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-cli    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aac    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      03                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aac    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se tipo codice di magazzino diverso da pro- *
      *                  * dotto di vendita : A controllo finale       *
      *                  *---------------------------------------------*
           if        d-pvg-age-tip-mag    not  = 01
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se codice numerico di magazzino a zero : A  *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-num-mag    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aap    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      02                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aap    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpx]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "UNODUE"             to   f-key                  .
           move      03                   to   rf-zpx-tip-inc         .
           move      d-pvg-age-cpv-aac    to   rf-zpx-cod-uno         .
           move      d-pvg-age-cpv-aap    to   rf-zpx-cod-due         .
           move      "pgm/age/fls/ioc/obj/iofzpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpx                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Determinazione della voce '% di sconto'     *
      *                  *---------------------------------------------*
           perform   det-per-sco-000      thru det-per-sco-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione % provvigioni in tabella     *
      *                  * [zpx]                                       *
      *                  *---------------------------------------------*
           move      w-det-per-sco-per    to   w-det-pps-zpx-psc      .
           move      d-pvg-age-tip-vpa    to   w-det-pps-zpx-vpa      .
           perform   det-pps-zpx-000      thru det-pps-zpx-999        .
       dpp-735.
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        w-det-pps-zpx-per    =    zero
                     go to dpp-800.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      w-det-pps-zpx-per    to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-740.
      *              *=================================================*
      *              * Codice di calcolo : % indirette per incrocio    *
      *              * tra :                                           *
      *              *                                                 *
      *              * - categoria provvigionale contenuta in anagra-  *
      *              *   fica agente                                   *
      *              * - categoria provvigionale contenuta in campagna *
      *              *   di vendita prodotto                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sub-routine                                 *
      *                  *---------------------------------------------*
           perform   dpp-074-000          thru dpp-074-999            .
      *                  *---------------------------------------------*
      *                  * Sub-routine di aggiornamento                *
      *                  *---------------------------------------------*
           perform   dpp-950              thru dpp-959                .
      *                  *---------------------------------------------*
      *                  * Se percentuali di provvigione tutte signi-  *
      *                  * ficative : uscita immediata                 *
      *                  *---------------------------------------------*
           if        w-det-age-iup-pvg    =    3
                     go to dpp-980.
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpp-800.
       dpp-800.
      *              *-------------------------------------------------*
      *              * Controllo finale                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione con il calcolo     *
      *                  * successivo in work di comodo                *
      *                  *---------------------------------------------*
           move      w-prs-dpp-obf-sdc
                    (w-det-age-ctr-001)   to   w-det-age-scd-pvg      .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore del se-   *
      *                  * gnale di continuazione                      *
      *                  *---------------------------------------------*
           if        w-det-age-scd-pvg    =    " "
                     go to dpp-810
           else if   w-det-age-scd-pvg    =    "+"
                     go to dpp-820
           else if   w-det-age-scd-pvg    =    "." or
                     w-det-age-scd-pvg    =    ":"
                     go to dpp-830
           else if   w-det-age-scd-pvg    =    "#" or
                     w-det-age-scd-pvg    =    "!"
                     go to dpp-840
           else if   w-det-age-scd-pvg    =    "z"
                     go to dpp-850.
       dpp-810.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Interrompi subi- *
      *                  * to il calcolo                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dpp-980.
       dpp-820.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua comun-  *
      *                  * que con il calcolo successivo               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione                    *
      *                      *-----------------------------------------*
           go to     dpp-050.
       dpp-830.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua con il  *
      *                  * calcolo successivo solamente se il calcolo  *
      *                  * in esame non ha prodotto una determinazione *
      *                  * di provvigioni, altrimenti interrompi       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se determinazione non effettuata : ri-  *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        w-det-age-fde-pvg    =    spaces
                     go to dpp-050.
      *                      *-----------------------------------------*
      *                      * Se determinazione non significativa :   *
      *                      * riciclo                                 *
      *                      *-----------------------------------------*
           if        w-det-age-fsd-pvg    =    spaces
                     go to dpp-050.
      *                      *-----------------------------------------*
      *                      * Se tipo continuazione pari a ':' si e-  *
      *                      * sce con il segnale di provvigione spe-  *
      *                      * ciale                                   *
      *                      *-----------------------------------------*
           if        w-det-age-scd-pvg    =    ":"
                     move  "#"            to   d-pvg-age-flg-pvs      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dpp-980.
       dpp-840.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua con il  *
      *                  * calcolo successivo solamente se se non e'   *
      *                  * stata trovata la tabella di riferimento,    *
      *                  * altrimenti interrompi                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se determinazione non effettuata : ri-  *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        w-det-age-fde-pvg    =    spaces
                     go to dpp-050.
      *                      *-----------------------------------------*
      *                      * Se tipo continuazione pari a '!' si e-  *
      *                      * sce con il segnale di provvigione spe-  *
      *                      * ciale                                   *
      *                      *-----------------------------------------*
           if        w-det-age-scd-pvg    =    "!"
                     move  "#"            to   d-pvg-age-flg-pvs      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dpp-980.
       dpp-850.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua con il  *
      *                  * calcolo successivo solamente se le percen-  *
      *                  * tuali di provvigione sinora cumulate sono   *
      *                  * pari a zero, altrimenti interrompi          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se indice su ultima provvigione    *
      *                      * significativa a zero                    *
      *                      *-----------------------------------------*
           if        w-det-age-iup-pvg    =    zero
                     go to dpp-050.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dpp-980.
       dpp-950.
      *              *=================================================*
      *              * Subroutine per aggiornamento % di provvigione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo per 3 provvigioni                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-age-ctr-002      .
       dpp-952.
           add       1                    to   w-det-age-ctr-002      .
           if        w-det-age-ctr-002    >    3
                     go to dpp-959.
      *                      *-----------------------------------------*
      *                      * Se % per aggiornamento a zero : riciclo *
      *                      *-----------------------------------------*
           if        w-det-age-agg-pvg
                    (w-det-age-ctr-002)   =    zero
                     go to dpp-952.
      *                      *-----------------------------------------*
      *                      * Incremento indice su ultima % signifi-  *
      *                      * cativa                                  *
      *                      *-----------------------------------------*
           add       1                    to   w-det-age-iup-pvg      .
      *                      *-----------------------------------------*
      *                      * Se oltre il massimo : a fine            *
      *                      *-----------------------------------------*
           if        w-det-age-iup-pvg    >    3
                     move  3              to   w-det-age-iup-pvg
                     go to dpp-959.
      *                      *-----------------------------------------*
      *                      * Memorizzazione % provvigione            *
      *                      *-----------------------------------------*
           move      w-det-age-agg-pvg
                    (w-det-age-ctr-002)   to   d-pvg-age-per-pvg
                                              (w-det-age-iup-pvg)     .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     dpp-952.
       dpp-959.
           exit.
       dpp-980.
      *              *=================================================*
      *              * Eventuali abbattimenti                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-abr-snx-abr    not  = "RI"
                     go to dpp-990.
      *                  *---------------------------------------------*
      *                  * Test se le provvigioni calcolate fino ad    *
      *                  * ora, sono speciali                          *
      *                  *---------------------------------------------*
           if        d-pvg-age-flg-pvs    =    "#"
                     go to dpp-990.
      *                  *---------------------------------------------*
      *                  * Accorpamento delle 3 provvigioni            *
      *                  *---------------------------------------------*
           move      d-pvg-age-per-pvg (1)
                                          to   w-det-per-uni-001      .
           move      d-pvg-age-per-pvg (2)
                                          to   w-det-per-uni-002      .
           move      d-pvg-age-per-pvg (3)
                                          to   w-det-per-uni-003      .
           move      zero                 to   w-det-per-uni-004      .
           move      zero                 to   w-det-per-uni-005      .
      *                  *---------------------------------------------*
      *                  * Subroutine di determinazione di una % unica *
      *                  *---------------------------------------------*
           perform   det-per-uni-000      thru det-per-uni-999        .
      *                  *---------------------------------------------*
      *                  * Valore determinato                          *
      *                  *---------------------------------------------*
           move      w-det-per-uni-per    to   w-det-psr-per-pvg      .
      *                  *---------------------------------------------*
      *                  * Eventuale abbattimento sulla singola riga,  *
      *                  * ed eventuale azzeramento determinazione     *
      *                  *                                             *
      *                  * ___ DA PERFEZIONARE (18/03/09) ___          *
      *                  *---------------------------------------------*
           perform   det-abt-rig-000      thru det-abt-rig-999        .
      *                  *---------------------------------------------*
      *                  * Provvigione abbattuta determinata           *
      *                  *---------------------------------------------*
           if        w-det-psr-per-pvg    not  = d-pvg-age-per-pvg (1)
                     move  "#"            to   d-pvg-age-flg-pvs      .
      *
           move      w-det-psr-per-pvg    to   d-pvg-age-per-pvg (1)  .
       dpp-990.
      *              *=================================================*
      *              * Eventuali operazioni in uscita                  *
      *              *-------------------------------------------------*
           go to     dpp-999.
       dpp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di provvigione                 *
      *    *                                                           *
      *    * Subroutine per il calcolo % di provvigioni indirette da   *
      *    * categoria in campagna di vendita prodotto                 *
      *    *                                                           *
      *    * Tipo calcolo : 52                                         *
      *    *-----------------------------------------------------------*
       dpp-052-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Categoria di comodo                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-age-cat-aap      .
       dpp-052-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice agente a zero : ad uscita         *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-age    =    zero
                     go to dpp-052-900.
       dpp-052-200.
      *                  *---------------------------------------------*
      *                  * Determinazione provvigioni per campagna di  *
      *                  * vendita attuale                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [lst]                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      03                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      c-sgl                to   rf-lst-sgl-vlt         .
           move      d-pvg-age-num-mag    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : oltre               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-052-400.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lst-dva-ini       >    d-pvg-age-dat-rif
                     go to dpp-052-400.
      *                      *-----------------------------------------*
      *                      * Test su data di fine campagna           *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       <    d-pvg-age-dat-rif
                     go to dpp-052-400.
      *                      *-----------------------------------------*
      *                      * Se Si/no utilizzo delle % di provvigio- *
      *                      * ne nella determinazione delle provvi-   *
      *                      * gioni contenuto nel record a No : oltre *
      *                      *-----------------------------------------*
           if        rf-lst-snx-pvg       not  = "S"
                     go to dpp-052-400.
      *                      *-----------------------------------------*
      *                      * Test se presente categoria provvigioni  *
      *                      *-----------------------------------------*
           if        rf-lst-cat-pvg       =    zero
                     go to dpp-052-400.
      *                      *-----------------------------------------*
      *                      * Categoria letta in comodo               *
      *                      *-----------------------------------------*
           move      rf-lst-cat-pvg       to   w-det-age-cat-aap      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     dpp-052-500.
       dpp-052-400.
      *                  *---------------------------------------------*
      *                  * Determinazione provvigioni per campagna di  *
      *                  * vendita storicizzata                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      03                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      c-sgl                to   rf-lsd-sgl-vlt         .
           move      d-pvg-age-num-mag    to   rf-lsd-num-pro         .
           move      d-pvg-age-dat-rif    to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-052-500.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-052-500.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 03                or
                     rf-lsd-cod-lst       not  = spaces            or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = c-sgl             or
                     rf-lsd-num-pro       not  = d-pvg-age-num-mag
                     go to dpp-052-500.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-ini       >    d-pvg-age-dat-rif
                     go to dpp-052-500.
      *                      *-----------------------------------------*
      *                      * Se Si/no utilizzo delle % di provvigio- *
      *                      * ne nella determinazione delle provvi-   *
      *                      * gioni contenuto nel record a No : A     *
      *                      * controllo finale                        *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-pvg       not  = "S"
                     go to dpp-052-500.
      *                      *-----------------------------------------*
      *                      * Test se presente categoria provvigioni  *
      *                      *-----------------------------------------*
           if        rf-lsd-cat-pvg       =    zero
                     go to dpp-052-500.
      *                      *-----------------------------------------*
      *                      * Categoria letta in comodo               *
      *                      *-----------------------------------------*
           move      rf-lsd-cat-pvg       to   w-det-age-cat-aap      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     dpp-052-500.
       dpp-052-500.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        w-det-age-cat-aap    =    zero
                     go to dpp-052-900.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      02                   to   rf-zpv-tip-cpv         .
           move      w-det-age-cat-aap    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-052-900.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Determinazione della voce '% di sconto'     *
      *                  *---------------------------------------------*
           perform   det-per-sco-000      thru det-per-sco-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione % provvigioni in tabella     *
      *                  * [zpv]                                       *
      *                  *---------------------------------------------*
           move      w-det-per-sco-per    to   w-det-pps-zpv-psc      .
           move      d-pvg-age-tip-vpa    to   w-det-pps-zpv-vpa      .
           perform   det-pps-zpv-000      thru det-pps-zpv-999        .
       dpp-052-600.
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
______*    if        w-det-pps-zpx-per    =    zero
______*              go to dpp-052-900.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      w-det-pps-zpv-per    to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
       dpp-052-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dpp-052-999.
       dpp-052-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di provvigione                 *
      *    *                                                           *
      *    * Subroutine per il calcolo % di provvigioni indirette da   *
      *    * categoria in anagrafica agente e categoria in campagna    *
      *    * prodotto                                                  *
      *    *                                                           *
      *    * Tipo calcolo : 74                                         *
      *    *-----------------------------------------------------------*
       dpp-074-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Categoria di comodo                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-age-cat-aap      .
       dpp-074-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice agente a zero : A controllo fina- *
      *                  * le                                          *
      *                  *---------------------------------------------*
           if        d-pvg-age-cod-age    =    zero
                     go to dpp-074-900.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        d-pvg-age-cpv-aaa    =    zero
                     go to dpp-074-900.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      01                   to   rf-zpv-tip-cpv         .
           move      d-pvg-age-cpv-aaa    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-074-900.
      *                  *---------------------------------------------*
      *                  * Se tipo codice di magazzino diverso da pro- *
      *                  * dotto di vendita : A controllo finale       *
      *                  *---------------------------------------------*
           if        d-pvg-age-tip-mag    not  = 01
                     go to dpp-074-900.
      *                  *---------------------------------------------*
      *                  * Se codice numerico di magazzino a zero : A  *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
           if        d-pvg-age-num-mag    =    zero
                     go to dpp-074-900.
       dpp-074-200.
      *                  *---------------------------------------------*
      *                  * Determinazione provvigioni per campagna di  *
      *                  * vendita attuale                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [lst]                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      03                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      c-sgl                to   rf-lst-sgl-vlt         .
           move      d-pvg-age-num-mag    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : oltre               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-074-400.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lst-dva-ini       >    d-pvg-age-dat-rif
                     go to dpp-074-400.
      *                      *-----------------------------------------*
      *                      * Test su data di fine campagna           *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       <    d-pvg-age-dat-rif
                     go to dpp-074-400.
      *                      *-----------------------------------------*
      *                      * Se Si/no utilizzo delle % di provvigio- *
      *                      * ne nella determinazione delle provvi-   *
      *                      * gioni contenuto nel record a No : oltre *
      *                      *-----------------------------------------*
           if        rf-lst-snx-pvg       not  = "S"
                     go to dpp-074-400.
      *                      *-----------------------------------------*
      *                      * Test se presente categoria provvigioni  *
      *                      *-----------------------------------------*
           if        rf-lst-cat-pvg       =    zero
                     go to dpp-074-400.
      *                      *-----------------------------------------*
      *                      * Categoria letta in comodo               *
      *                      *-----------------------------------------*
           move      rf-lst-cat-pvg       to   w-det-age-cat-aap      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     dpp-074-500.
       dpp-074-400.
      *                  *---------------------------------------------*
      *                  * Determinazione provvigioni per campagna di  *
      *                  * vendita storicizzata                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      03                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      c-sgl                to   rf-lsd-sgl-vlt         .
           move      d-pvg-age-num-mag    to   rf-lsd-num-pro         .
           move      d-pvg-age-dat-rif    to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-074-500.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-074-500.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 03                or
                     rf-lsd-cod-lst       not  = spaces            or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = c-sgl             or
                     rf-lsd-num-pro       not  = d-pvg-age-num-mag
                     go to dpp-074-500.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-ini       >    d-pvg-age-dat-rif
                     go to dpp-074-500.
      *                      *-----------------------------------------*
      *                      * Se Si/no utilizzo delle % di provvigio- *
      *                      * ne nella determinazione delle provvi-   *
      *                      * gioni contenuto nel record a No : A     *
      *                      * controllo finale                        *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-pvg       not  = "S"
                     go to dpp-074-500.
      *                      *-----------------------------------------*
      *                      * Test se presente categoria provvigioni  *
      *                      *-----------------------------------------*
           if        rf-lsd-cat-pvg       =    zero
                     go to dpp-074-500.
      *                      *-----------------------------------------*
      *                      * Categoria letta in comodo               *
      *                      *-----------------------------------------*
           move      rf-lsd-cat-pvg       to   w-det-age-cat-aap      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     dpp-074-500.
       dpp-074-500.
      *                  *---------------------------------------------*
      *                  * Se categoria provvigionale a zero : A con-  *
      *                  * trollo finale                               *
      *                  *---------------------------------------------*
           if        w-det-age-cat-aap    =    zero
                     go to dpp-074-900.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpv]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCPV"             to   f-key                  .
           move      02                   to   rf-zpv-tip-cpv         .
           move      w-det-age-cat-aap    to   rf-zpv-cod-cpv         .
           move      "pgm/age/fls/ioc/obj/iofzpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpv                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-074-900.
      *                  *---------------------------------------------*
      *                  * Lettura file [zpx]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "UNODUE"             to   f-key                  .
           move      01                   to   rf-zpx-tip-inc         .
           move      d-pvg-age-cpv-aaa    to   rf-zpx-cod-uno         .
           move      w-det-age-cat-aap    to   rf-zpx-cod-due         .
           move      "pgm/age/fls/ioc/obj/iofzpx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpx                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : A controllo finale      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpp-074-900.
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata a : Si    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fde-pvg      .
      *                  *---------------------------------------------*
      *                  * Determinazione della voce '% di sconto'     *
      *                  *---------------------------------------------*
           perform   det-per-sco-000      thru det-per-sco-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione % provvigioni in tabella     *
      *                  * [zpx]                                       *
      *                  *---------------------------------------------*
           move      w-det-per-sco-per    to   w-det-pps-zpx-psc      .
           move      d-pvg-age-tip-vpa    to   w-det-pps-zpx-vpa      .
           perform   det-pps-zpx-000      thru det-pps-zpx-999        .
       dpp-074-600.
      *                  *---------------------------------------------*
      *                  * Se percentuale di provvigione a zero : a    *
      *                  * controllo finale                            *
      *                  *---------------------------------------------*
______*    if        w-det-pps-zpx-per    =    zero
______*              go to dpp-074-900.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' determinazione ef- *
      *                  * fettuata a : Si                             *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-age-fsd-pvg      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento percentuali di provvigione    *
      *                  *---------------------------------------------*
           move      w-det-pps-zpx-per    to   w-det-age-agg-pvg (1)  .
           move      zero                 to   w-det-age-agg-pvg (2)  .
           move      zero                 to   w-det-age-agg-pvg (3)  .
       dpp-074-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dpp-074-999.
       dpp-074-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Determinazione % di provvi-   *
      *    *                             gione in ordini-bolle-fatture *
      *    *-----------------------------------------------------------*
       prs-dpp-obf-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero elementi in tabella     *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-dpp-obf-ele      .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age[dpp-obf]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to prs-dpp-obf-800.
      *              *-------------------------------------------------*
      *              * Valore referenza in comodo ridefinito           *
      *              *-------------------------------------------------*
           move      s-alf                to   w-prs-dpp-obf-wst      .
       prs-dpp-obf-100.
      *              *-------------------------------------------------*
      *              * Test se valore a spazi                          *
      *              *-------------------------------------------------*
           if        w-prs-dpp-obf-wst    =    spaces
                     go to prs-dpp-obf-800.
       prs-dpp-obf-110.
      *              *-------------------------------------------------*
      *              * Controllo su metodo di determinazione della %   *
      *              * di sconto                                       *
      *              *-------------------------------------------------*
           if        w-prs-dpp-obf-wmd    =    01 or
                     w-prs-dpp-obf-wmd    =    02 or
                     w-prs-dpp-obf-wmd    =    03
                     go to prs-dpp-obf-115.
      *                  *---------------------------------------------*
      *                  * Normalizzazione metodo di determinazione    *
      *                  * della % di sconto                           *
      *                  *---------------------------------------------*
           move      01                   to   w-prs-dpp-obf-wmd      .
       prs-dpp-obf-115.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione stringa letta                *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-dpp-obf-c01      .
       prs-dpp-obf-120.
           add       1                    to   w-prs-dpp-obf-c01      .
           if        w-prs-dpp-obf-c01    >    w-prs-dpp-obf-max
                     go to prs-dpp-obf-800.
      *                  *---------------------------------------------*
      *                  * Controllo formale se valore ammissibile     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Su codice di calcolo                    *
      *                      *-----------------------------------------*
           if        w-prs-dpp-obf-aaa
                    (w-prs-dpp-obf-c01)   not  numeric
                     go to prs-dpp-obf-120.
      *                  *---------------------------------------------*
      *                  * Codice di calcolo in work di comodo         *
      *                  *---------------------------------------------*
           move      w-prs-dpp-obf-aaa
                    (w-prs-dpp-obf-c01)   to   w-prs-dpp-obf-wcc      .
      *                  *---------------------------------------------*
      *                  * Controllo su valori ammessi                 *
      *                  *                                             *
      *                  * Valori ammessi :                            *
      *                  *                                             *
      *                  *  - 00 = Nessun calcolo                      *
      *                  *                                             *
      *                  *  - 11 = % dirette da anagrafica agente      *
      *                  *                                             *
      *                  *  - 12 = % indirette da categoria provvigio- *
      *                  *         nale in anagrafica agente           *
      *                  *                                             *
      *                  *  - 21 = % dirette da anagrafica prodotto    *
      *                  *                                             *
      *                  *  - 22 = % indirette da categoria provvigio- *
      *                  *         nale in anagrafica prodotto         *
      *                  *                                             *
      *                  *  - 31 = % dirette da anagrafica commerciale *
      *                  *         cliente                             *
      *                  *                                             *
      *                  *  - 32 = % indirette da categoria provvigio- *
      *                  *         nale in anagrafica commerciale cli- *
      *                  *         ente                                *
      *                  *                                             *
      *                  *  - 41 = % dirette contenute nei prezzi net- *
      *                  *         ti concordati per cliente           *
      *                  *                                             *
      *                  *  - 51 = % dirette contenute nei prezzi per  *
      *                  *         campagna di vendita                 *
      *                  *                                             *
      *                  *  - 61 = % dirette contenute nei prezzi di   *
      *                  *         listino di vendita                  *
      *                  *                                             *
      *                  *  - 71 = % indirette per incrocio tra        *
      *                  *           - categoria provvigionale conte-  *
      *                  *             nuta in anagrafica agente       *
      *                  *           - categoria provvigionale conte-  *
      *                  *             nuta in anagrafica prodotto     *
      *                  *                                             *
      *                  *  - 72 = % indirette per incrocio tra        *
      *                  *           - categoria provvigionale conte-  *
      *                  *             nuta in anagrafica agente       *
      *                  *           - categoria provvigionale conte-  *
      *                  *             nuta in anagrafica commerciale  *
      *                  *             cliente                         *
      *                  *                                             *
      *                  *  - 73 = % indirette per incrocio tra        *
      *                  *           - categoria provvigionale conte-  *
      *                  *             nuta in anagrafica commerciale  *
      *                  *             cliente                         *
      *                  *           - categoria provvigionale conte-  *
      *                  *             nuta in anagrafica prodotto     *
      *                  *                                             *
      *                  *---------------------------------------------*
           if        w-prs-dpp-obf-wcc    not  = 00 and
                     w-prs-dpp-obf-wcc    not  = 11 and
                     w-prs-dpp-obf-wcc    not  = 12 and
                     w-prs-dpp-obf-wcc    not  = 21 and
                     w-prs-dpp-obf-wcc    not  = 22 and
                     w-prs-dpp-obf-wcc    not  = 31 and
                     w-prs-dpp-obf-wcc    not  = 32 and
                     w-prs-dpp-obf-wcc    not  = 41 and
                     w-prs-dpp-obf-wcc    not  = 51 and
                     w-prs-dpp-obf-wcc    not  = 52 and
                     w-prs-dpp-obf-wcc    not  = 61 and
                     w-prs-dpp-obf-wcc    not  = 71 and
                     w-prs-dpp-obf-wcc    not  = 72 and
                     w-prs-dpp-obf-wcc    not  = 73 and
                     w-prs-dpp-obf-wcc    not  = 74
                     go to prs-dpp-obf-120.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione calcolo in work di *
      *                  * comodo                                      *
      *                  *---------------------------------------------*
           move      w-prs-dpp-obf-bbb
                    (w-prs-dpp-obf-c01)   to   w-prs-dpp-obf-wsc      .
      *                  *---------------------------------------------*
      *                  * Controllo su valori ammessi                 *
      *                  *                                             *
      *                  * Valori ammessi :                            *
      *                  *                                             *
      *                  *  - " " : Interrompi subito il calcolo       *
      *                  *                                             *
      *                  *  - "+" : Continua comunque con il calcolo   *
      *                  *          successivo                         *
      *                  *                                             *
      *                  *  - "." : Continua con il calcolo successivo *
      *                  *          solamente se il calcolo in esame   *
      *                  *          non ha prodotto una determinazione *
      *                  *          di provvigioni, altrimenti inter-  *
      *                  *          rompi il calcolo                   *
      *                  *                                             *
      *                  *  - ":" : Come il caso precedente, pero' le  *
      *                  *          provvigioni vengono considerate    *
      *                  *          speciali, ovvero non saranno sot-  *
      *                  *          toposte ad eventuali abbattimenti  *
      *                  *          in riga                            *
      *                  *                                             *
      *                  *  - "#" : Continua con il calcolo successivo *
      *                  *          solamente se non e' stata trovata  *
      *                  *          la tabella di riferimento, altri-  *
      *                  *          menti interrompi; valido solo per  *
      *                  *          i tipi calcolo indiretti           *
      *                  *                                             *
      *                  *  - "!" : Come il caso precedente, pero' le  *
      *                  *          provvigioni vengono considerate    *
      *                  *          speciali, ovvero non saranno sot-  *
      *                  *          toposte ad eventuali abbattimenti  *
      *                  *          in riga                            *
      *                  *                                             *
      *                  *  - "z" : Continua con il calcolo successivo *
      *                  *          solamente se le percentuali di     *
      *                  *          provvigione sinora cumulate sono   *
      *                  *          pari a zero, altrimenti interrompi *
      *                  *                                             *
      *                  *---------------------------------------------*
           if        w-prs-dpp-obf-wsc    not  = " " and
                     w-prs-dpp-obf-wsc    not  = "+" and
                     w-prs-dpp-obf-wsc    not  = "." and
                     w-prs-dpp-obf-wsc    not  = ":" and
                     w-prs-dpp-obf-wsc    not  = "#" and
                     w-prs-dpp-obf-wsc    not  = "!" and
                     w-prs-dpp-obf-wsc    not  = "z"
                     go to prs-dpp-obf-120.
      *                  *---------------------------------------------*
      *                  * Test se codice di calcolo gia' esistente    *
      *                  * fra quelli memorizzati                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-prs-dpp-obf-c02      .
       prs-dpp-obf-140.
           add       1                    to   w-prs-dpp-obf-c02      .
           if        w-prs-dpp-obf-c02    >    w-prs-dpp-obf-ele
                     go to prs-dpp-obf-160.
           if        w-prs-dpp-obf-wcc    =    w-prs-dpp-obf-cdc
                                              (w-prs-dpp-obf-c02)
                     go to prs-dpp-obf-120
           else      go to prs-dpp-obf-140.
       prs-dpp-obf-160.
      *                  *---------------------------------------------*
      *                  * Memorizzazione elemento in tabella          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su contatore elementi bufferizzati *
      *                      *-----------------------------------------*
           if        w-prs-dpp-obf-ele    not  < w-prs-dpp-obf-max
                     go to prs-dpp-obf-800.
      *                      *-----------------------------------------*
      *                      * Incremento contatore elementi bufferiz- *
      *                      * zati                                    *
      *                      *-----------------------------------------*
           add       1                    to   w-prs-dpp-obf-ele      .
      *                      *-----------------------------------------*
      *                      * Memorizzazione elemento                 *
      *                      *-----------------------------------------*
           move      w-prs-dpp-obf-wcc    to   w-prs-dpp-obf-cdc
                                              (w-prs-dpp-obf-ele)     .
           move      w-prs-dpp-obf-wsc    to   w-prs-dpp-obf-sdc
                                              (w-prs-dpp-obf-ele)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione stringa letta      *
      *                      *-----------------------------------------*
           go to     prs-dpp-obf-120.
       prs-dpp-obf-800.
      *              *-------------------------------------------------*
      *              * Test su numero elementi bufferizzati            *
      *              *-------------------------------------------------*
           if        w-prs-dpp-obf-ele    not  = zero
                     go to prs-dpp-obf-999.
      *              *-------------------------------------------------*
      *              * Se zero elementi bufferizzati                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore di default                           *
      *                  *---------------------------------------------*
           move      "41.51.73.71.72.21.22.31.32.11.12                02
      -              " "                  to   w-prs-dpp-obf-wst      .
      *                  *---------------------------------------------*
      *                  * Riciclo a rielaborazione stringa            *
      *                  *---------------------------------------------*
           go to     prs-dpp-obf-100.
       prs-dpp-obf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-dpp-obf-999.
       prs-dpp-obf-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione relativa agli abbatti-    *
      *    * menti di provvigione in riga                              *
      *    *-----------------------------------------------------------*
       prs-age-abr-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age/age300[abt-rig]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-age-abr-300.
       prs-age-abr-100.
      *              *-------------------------------------------------*
      *              * Se personalizzazione non esistente              *
      *              *-------------------------------------------------*
       prs-age-abr-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione parametri                   *
      *                  *---------------------------------------------*
           move      "NO"                 to   w-prs-abr-snx-abr      .
           move      00                   to   w-prs-abr-det-pse      .
           move      000                  to   w-prs-abr-mab-sco      .
           move      0                    to   w-prs-abr-mab-scd      .
           move      000                  to   w-prs-abr-mau-mag      .
           move      00                   to   w-prs-abr-max-psa      .
           move      00                   to   w-prs-abr-max-psc      .
       prs-age-abr-200.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-age-abr-900.
       prs-age-abr-300.
      *              *-------------------------------------------------*
      *              * Se personalizzazione esistente                  *
      *              *-------------------------------------------------*
       prs-age-abr-350.
      *                  *---------------------------------------------*
      *                  * Spostamento valore personalizzazione in a-  *
      *                  * rea di destinazione                         *
      *                  *---------------------------------------------*
           move      s-alf                to   w-prs-abr              .
       prs-age-abr-400.
      *                  *---------------------------------------------*
      *                  * Controllo parametri                         *
      *                  *---------------------------------------------*
           if        w-prs-abr-snx-abr    not  = "NO" and
                     w-prs-abr-snx-abr    not  = "SI" and
                     w-prs-abr-snx-abr    not  = "RI" and
                     w-prs-abr-snx-abr    not  = "F1" and
                     w-prs-abr-snx-abr    not  = "F2" and
                     w-prs-abr-snx-abr    not  = "F3" and
                     w-prs-abr-snx-abr    not  = "F4" and
                     w-prs-abr-snx-abr    not  = "F5" and
                     w-prs-abr-snx-abr    not  = "F6" and
                     w-prs-abr-snx-abr    not  = "F7" and
                     w-prs-abr-snx-abr    not  = "F8" and
                     w-prs-abr-snx-abr    not  = "F9"
                     go to prs-age-abr-100.
      *
           if        w-prs-abr-det-pse    not  numeric
                     go to prs-age-abr-100.
           if        w-prs-abr-det-pse    not  = 01 and
                     w-prs-abr-det-pse    not  = 02 and
                     w-prs-abr-det-pse    not  = 03
                     go to prs-age-abr-100.
      *
           if        w-prs-abr-mab-sco    not  numeric
                     move  000            to   w-prs-abr-mab-sco
                     move  0              to   w-prs-abr-mab-scd      .
           if        w-prs-abr-mab-sco    <    100 or
                     w-prs-abr-mab-sco    >    199
                     move  000            to   w-prs-abr-mab-sco
                     move  0              to   w-prs-abr-mab-scd      .
           if        w-prs-abr-mab-scd    not  numeric
                     move  0              to   w-prs-abr-mab-scd      .
      *
           if        w-prs-abr-mau-mag    not  numeric
                     move  000            to   w-prs-abr-mau-mag      .
           if        w-prs-abr-mau-mag    <    100 or
                     w-prs-abr-mau-mag    >    199
                     move  000            to   w-prs-abr-mau-mag      .
      *
           if        w-prs-abr-max-psa    not  numeric
                     move  00             to   w-prs-abr-max-psa      .
           if        w-prs-abr-max-psa    =    zero
                     move  50             to   w-prs-abr-max-psa      .
      *
           if        w-prs-abr-max-psc    not  numeric
                     move  00             to   w-prs-abr-max-psc      .
      *
           if        w-prs-abr-mab-sco    =    zero and
                     w-prs-abr-mau-mag    =    zero
                     go to prs-age-abr-100.
       prs-age-abr-450.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-age-abr-900.
       prs-age-abr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-age-abr-999.
       prs-age-abr-999.
           exit.

      *    *===========================================================*
      *    * Determinazione della voce 'percentuale di sconto' in base *
      *    * a quanto stabilito dalle personalizzazioni                *
      *    *                                                           *
      *    * Output : w-det-per-sco-per     = Percentuale di sconto    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-per-sco-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di output                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-per-sco-per      .
       det-per-sco-010.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del valore espresso nel- *
      *              * la personalizzazione                            *
      *              *-------------------------------------------------*
           if        w-prs-dpp-obf-wmd    =    01
                     go to det-per-sco-100
           else if   w-prs-dpp-obf-wmd    =    02
                     go to det-per-sco-200
           else if   w-prs-dpp-obf-wmd    =    03
                     go to det-per-sco-300
           else      go to det-per-sco-900.
       det-per-sco-100.
      *              *-------------------------------------------------*
      *              * La % di sconto applicata deve essere desunta    *
      *              * dalla proporzione tra prezzo lordo standard e   *
      *              * prezzo netto effettivamente praticato           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione della % di sconto in base al *
      *                  * confronto tra due prezzi                    *
      *                  *---------------------------------------------*
           move      d-pvg-age-prz-lrs    to   w-det-pes-prz-pi1      .
           move      d-pvg-age-prz-net    to   w-det-pes-prz-pi2      .
           perform   det-pes-prz-000      thru det-pes-prz-999        .
      *                  *---------------------------------------------*
      *                  * Valore determinato in campo di output       *
      *                  *---------------------------------------------*
           move      w-det-pes-prz-sco    to   w-det-per-sco-per      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-per-sco-900.
       det-per-sco-200.
      *              *-------------------------------------------------*
      *              * La % di sconto applicata deve essere desunta    *
      *              * dalla proporzione tra prezzo netto standard e   *
      *              * prezzo netto effettivamente praticato           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione della % di sconto in base al *
      *                  * confronto tra due prezzi                    *
      *                  *---------------------------------------------*
           move      d-pvg-age-prz-nts    to   w-det-pes-prz-pi1      .
           move      d-pvg-age-prz-net    to   w-det-pes-prz-pi2      .
           perform   det-pes-prz-000      thru det-pes-prz-999        .
      *                  *---------------------------------------------*
      *                  * Valore determinato in campo di output       *
      *                  *---------------------------------------------*
           move      w-det-pes-prz-sco    to   w-det-per-sco-per      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-per-sco-900.
       det-per-sco-300.
      *              *-------------------------------------------------*
      *              * La % di sconto applicata deve essere desunta    *
      *              * dalla sommatoria scalare dei max 5 sconti ef-   *
      *              * fettivamente praticati, senza tener conto del   *
      *              * prezzo                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accorpamento dei 5 sconti provenienti dal-  *
      *                  * l'area di comunicazione                     *
      *                  *---------------------------------------------*
           move      d-pvg-age-per-scr (1)
                                          to   w-det-per-uni-001      .
           move      d-pvg-age-per-scr (2)
                                          to   w-det-per-uni-002      .
           move      d-pvg-age-per-scr (3)
                                          to   w-det-per-uni-003      .
           move      d-pvg-age-per-scr (4)
                                          to   w-det-per-uni-004      .
           move      d-pvg-age-per-scr (5)
                                          to   w-det-per-uni-005      .
      *                  *---------------------------------------------*
      *                  * Subroutine di determinazione di una % unica *
      *                  *---------------------------------------------*
           perform   det-per-uni-000      thru det-per-uni-999        .
      *                  *---------------------------------------------*
      *                  * Valore determinato in campo di output       *
      *                  *---------------------------------------------*
           move      w-det-per-uni-per    to   w-det-per-sco-per      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-per-sco-900.
       det-per-sco-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-per-sco-999.
       det-per-sco-999.
           exit.

      *    *===========================================================*
      *    * Determinazione % unica da max 5 % distinte                *
      *    *                                                           *
      *    * Input  : w-det-per-uni-001     = 1.a percentuale          *
      *    *          w-det-per-uni-002     = 2.a percentuale          *
      *    *          w-det-per-uni-003     = 3.a percentuale          *
      *    *          w-det-per-uni-004     = 4.a percentuale          *
      *    *          w-det-per-uni-005     = 5.a percentuale          *
      *    *                                                           *
      *    * Output : w-det-per-uni-per     = Unica percentuale        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-per-uni-000.
           move      100                  to   w-det-per-uni-w01      .
           move      zero                 to   w-det-per-uni-w02      .
      *
           multiply  w-det-per-uni-001    by   w-det-per-uni-w01
                                        giving w-det-per-uni-w03      .
           divide    100                  into w-det-per-uni-w03      .
           subtract  w-det-per-uni-w03    from w-det-per-uni-w01      .
           add       w-det-per-uni-w03    to   w-det-per-uni-w02      .
      *
           multiply  w-det-per-uni-002    by   w-det-per-uni-w01
                                        giving w-det-per-uni-w03      .
           divide    100                  into w-det-per-uni-w03      .
           subtract  w-det-per-uni-w03    from w-det-per-uni-w01      .
           add       w-det-per-uni-w03    to   w-det-per-uni-w02      .
      *
           multiply  w-det-per-uni-003    by   w-det-per-uni-w01
                                        giving w-det-per-uni-w03      .
           divide    100                  into w-det-per-uni-w03      .
           subtract  w-det-per-uni-w03    from w-det-per-uni-w01      .
           add       w-det-per-uni-w03    to   w-det-per-uni-w02      .
      *
           multiply  w-det-per-uni-004    by   w-det-per-uni-w01
                                        giving w-det-per-uni-w03      .
           divide    100                  into w-det-per-uni-w03      .
           subtract  w-det-per-uni-w03    from w-det-per-uni-w01      .
           add       w-det-per-uni-w03    to   w-det-per-uni-w02      .
      *
           multiply  w-det-per-uni-005    by   w-det-per-uni-w01
                                        giving w-det-per-uni-w03      .
           divide    100                  into w-det-per-uni-w03      .
           subtract  w-det-per-uni-w03    from w-det-per-uni-w01      .
           add       w-det-per-uni-w03    to   w-det-per-uni-w02      .
      *
           move      w-det-per-uni-w02    to   w-det-per-uni-per      .
       det-per-uni-999.
           exit.

      *    *===========================================================*
      *    * Determinazione della % di sconto in base al confronto tra *
      *    * due prezzi : si tratta cioe' di determinare la % di sco-  *
      *    * stamento del secondo prezzo rispetto al primo             *
      *    *-----------------------------------------------------------*
       det-pes-prz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-pes-prz-s11      .
           move      zero                 to   w-det-pes-prz-s13      .
           move      zero                 to   w-det-pes-prz-sco      .
      *              *-------------------------------------------------*
      *              * Se prezzo 1 a zero : uscita con zero            *
      *              *-------------------------------------------------*
           if        w-det-pes-prz-pi1    =    zero
                     go to det-pes-prz-900.
      *              *-------------------------------------------------*
      *              * Se prezzo 2 a zero : uscita con zero            *
      *              *-------------------------------------------------*
           if        w-det-pes-prz-pi2    =    zero
                     go to det-pes-prz-900.
      *              *-------------------------------------------------*
      *              * Se prezzo 2 > di prezzo 1 : uscita con zero     *
      *              *-------------------------------------------------*
           if        w-det-pes-prz-pi2    >    w-det-pes-prz-pi1
                     go to det-pes-prz-900.
      *              *-------------------------------------------------*
      *              * Calcolo della percentuale                       *
      *              *-------------------------------------------------*
           move      w-det-pes-prz-pi2    to   w-det-pes-prz-s13      .
           multiply  100                  by   w-det-pes-prz-s13      .
           divide    w-det-pes-prz-pi1    into w-det-pes-prz-s13
                                        giving w-det-pes-prz-s11
                                                         rounded      .
      *              *-------------------------------------------------*
      *              * % calcolata in campo di destinazione            *
      *              *-------------------------------------------------*
           subtract  w-det-pes-prz-s11    from 100
                                        giving w-det-pes-prz-sco      .
       det-pes-prz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-pes-prz-999.
       det-pes-prz-999.
           exit.

      *    *===========================================================*
      *    * Determinazione % provvigioni in tabella categoria legata  *
      *    * ad Agente, Cliente o Prodotto                             *
      *    *                                                           *
      *    * Input  : rf-zpv                = Tabella provvigioni      *
      *    *          w-det-pps-zpv-psc     = % di sconto              *
      *    *          w-det-pps-zpv-vpa     = Tipo vendita             *
      *    *                                                           *
      *    * Output : w-det-pps-zpv-per     = % di provvigioni         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-pps-zpv-000.
      *              *-------------------------------------------------*
      *              * Test se stessa scaletta provvigioni dirette e   *
      *              * indirette                                       *
      *              *-------------------------------------------------*
           if        rf-zpv-trt-pvi       =    01
                     go to det-pps-zpv-100.
      *              *-------------------------------------------------*
      *              * Deviazione in presenza del trattamento di prov- *
      *              * vigioni dirette od indirette                    *
      *              *-------------------------------------------------*
           if        w-det-pps-zpv-vpa    =    02
                     go to det-pps-zpv-500.
       det-pps-zpv-100.
      *              *-------------------------------------------------*
      *              * Se provvigioni dirette                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scansione degli scaglioni di sconto         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazioni preliminari             *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-pps-zpv-per      .
           move      zero                 to   w-det-pps-zpv-inx      .
           move      zero                 to   w-det-pps-zpv-ies      .
       det-pps-zpv-200.
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione                      *
      *                      *-----------------------------------------*
           add       1                    to   w-det-pps-zpv-inx      .
      *                      *-----------------------------------------*
      *                      * Se ultimo elemento : a test su indice   *
      *                      *-----------------------------------------*
           if        w-det-pps-zpv-inx    >    6
                     go to det-pps-zpv-300.
      *                      *-----------------------------------------*
      *                      * Se elemento vuoto : riciclo             *
      *                      *-----------------------------------------*
           if        rf-zpv-spd-scu
                    (w-det-pps-zpv-inx)   =    zero  and
                     rf-zpv-spd-pvg
                    (w-det-pps-zpv-inx, 1)
                                          =    zero
                     go to det-pps-zpv-200.
      *                      *-----------------------------------------*
      *                      * Confronto tra % di sconto               *
      *                      *-----------------------------------------*
           if        rf-zpv-spd-scu
                    (w-det-pps-zpv-inx)   >    w-det-pps-zpv-psc
                     go to det-pps-zpv-200.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dell'indice             *
      *                      *-----------------------------------------*
           move      w-det-pps-zpv-inx    to   w-det-pps-zpv-ies      .
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione                    *
      *                      *-----------------------------------------*
           go to     det-pps-zpv-200.
       det-pps-zpv-300.
      *                  *---------------------------------------------*
      *                  * Test su indice determinato                  *
      *                  *---------------------------------------------*
           if        w-det-pps-zpv-ies    =    zero
                     go to det-pps-zpv-400.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento buono              *
      *                  *---------------------------------------------*
           move      rf-zpv-spd-pvg
                    (w-det-pps-zpv-ies, 1)
                                          to   w-det-pps-zpv-per      .
       det-pps-zpv-400.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-pps-zpv-900.
       det-pps-zpv-500.
      *              *-------------------------------------------------*
      *              * Se provvigioni indirette                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scansione degli scaglioni di sconto         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazioni preliminari             *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-pps-zpv-per      .
           move      zero                 to   w-det-pps-zpv-inx      .
           move      zero                 to   w-det-pps-zpv-ies      .
       det-pps-zpv-600.
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione                      *
      *                      *-----------------------------------------*
           add       1                    to   w-det-pps-zpv-inx      .
      *                      *-----------------------------------------*
      *                      * Se ultimo elemento : a test su indice   *
      *                      *-----------------------------------------*
           if        w-det-pps-zpv-inx    >    6
                     go to det-pps-zpv-700.
      *                      *-----------------------------------------*
      *                      * Se elemento vuoto : riciclo             *
      *                      *-----------------------------------------*
           if        rf-zpv-spi-scu
                    (w-det-pps-zpv-inx)   =    zero  and
                     rf-zpv-spi-pvg
                    (w-det-pps-zpv-inx, 1)
                                          =    zero
                     go to det-pps-zpv-600.
      *                      *-----------------------------------------*
      *                      * Confronto tra % di sconto               *
      *                      *-----------------------------------------*
           if        rf-zpv-spi-scu
                    (w-det-pps-zpv-inx)   >    w-det-pps-zpv-psc
                     go to det-pps-zpv-600.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dell'indice             *
      *                      *-----------------------------------------*
           move      w-det-pps-zpv-inx    to   w-det-pps-zpv-ies      .
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione                    *
      *                      *-----------------------------------------*
           go to     det-pps-zpv-600.
       det-pps-zpv-700.
      *                  *---------------------------------------------*
      *                  * Test su indice determinato                  *
      *                  *---------------------------------------------*
           if        w-det-pps-zpv-ies    =    zero
                     go to det-pps-zpv-800.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento buono              *
      *                  *---------------------------------------------*
           move      rf-zpv-spi-pvg
                    (w-det-pps-zpv-ies, 1)
                                          to   w-det-pps-zpv-per      .
       det-pps-zpv-800.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-pps-zpv-900.
       det-pps-zpv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-pps-zpv-999.
       det-pps-zpv-999.
           exit.

      *    *===========================================================*
      *    * Determinazione % provvigioni in tabella categorie incro-  *
      *    * ciate tra Agente-Cliente, Agente-Prodotto o Cliente-Pro-  *
      *    * dotto                                                     *
      *    *                                                           *
      *    * Input  : rf-zpx                = Tabella provvigioni      *
      *    *          w-det-pps-zpx-psc     = % di sconto              *
      *    *          w-det-pps-zpx-vpa     = Tipo vendita             *
      *    *                                                           *
      *    * Output : w-det-pps-zpx-per     = % di provvigioni         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-pps-zpx-000.
      *              *-------------------------------------------------*
      *              * Test se stessa scaletta provvigioni dirette e   *
      *              * indirette                                       *
      *              *-------------------------------------------------*
           if        rf-zpx-trt-pvi       =    01
                     go to det-pps-zpx-100.
      *              *-------------------------------------------------*
      *              * Deviazione in presenza del trattamento di prov- *
      *              * vigioni dirette od indirette                    *
      *              *-------------------------------------------------*
           if        w-det-pps-zpx-vpa    =    02
                     go to det-pps-zpx-500.
       det-pps-zpx-100.
      *              *-------------------------------------------------*
      *              * Se provvigioni dirette                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scansione degli scaglioni di sconto         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazioni preliminari             *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-pps-zpx-per      .
           move      zero                 to   w-det-pps-zpx-inx      .
           move      zero                 to   w-det-pps-zpx-ies      .
       det-pps-zpx-200.
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione                      *
      *                      *-----------------------------------------*
           add       1                    to   w-det-pps-zpx-inx      .
      *                      *-----------------------------------------*
      *                      * Se ultimo elemento : a test su indice   *
      *                      *-----------------------------------------*
           if        w-det-pps-zpx-inx    >    6
                     go to det-pps-zpx-300.
      *                      *-----------------------------------------*
      *                      * Se elemento vuoto : riciclo             *
      *                      *-----------------------------------------*
           if        rf-zpx-spd-scu
                    (w-det-pps-zpx-inx)   =    zero  and
                     rf-zpx-spd-pvg
                    (w-det-pps-zpx-inx, 1)
                                          =    zero
                     go to det-pps-zpx-200.
      *                      *-----------------------------------------*
      *                      * Confronto tra % di sconto               *
      *                      *-----------------------------------------*
           if        rf-zpx-spd-scu
                    (w-det-pps-zpx-inx)   >    w-det-pps-zpx-psc
                     go to det-pps-zpx-200.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dell'indice             *
      *                      *-----------------------------------------*
           move      w-det-pps-zpx-inx    to   w-det-pps-zpx-ies      .
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione                    *
      *                      *-----------------------------------------*
           go to     det-pps-zpx-200.
       det-pps-zpx-300.
      *                  *---------------------------------------------*
      *                  * Test su indice determinato                  *
      *                  *---------------------------------------------*
           if        w-det-pps-zpx-ies    =    zero
                     go to det-pps-zpx-400.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento buono              *
      *                  *---------------------------------------------*
           move      rf-zpx-spd-pvg
                    (w-det-pps-zpx-ies, 1)
                                          to   w-det-pps-zpx-per      .
       det-pps-zpx-400.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-pps-zpx-900.
       det-pps-zpx-500.
      *              *-------------------------------------------------*
      *              * Se provvigioni indirette                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scansione degli scaglioni di sconto         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazioni preliminari             *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-pps-zpx-per      .
           move      zero                 to   w-det-pps-zpx-inx      .
           move      zero                 to   w-det-pps-zpx-ies      .
       det-pps-zpx-600.
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione                      *
      *                      *-----------------------------------------*
           add       1                    to   w-det-pps-zpx-inx      .
      *                      *-----------------------------------------*
      *                      * Se ultimo elemento : a test su indice   *
      *                      *-----------------------------------------*
           if        w-det-pps-zpx-inx    >    6
                     go to det-pps-zpx-700.
      *                      *-----------------------------------------*
      *                      * Se elemento vuoto : riciclo             *
      *                      *-----------------------------------------*
           if        rf-zpx-spi-scu
                    (w-det-pps-zpx-inx)   =    zero  and
                     rf-zpx-spi-pvg
                    (w-det-pps-zpx-inx, 1)
                                          =    zero
                     go to det-pps-zpx-600.
      *                      *-----------------------------------------*
      *                      * Confronto tra % di sconto               *
      *                      *-----------------------------------------*
           if        rf-zpx-spi-scu
                    (w-det-pps-zpx-inx)   >    w-det-pps-zpx-psc
                     go to det-pps-zpx-600.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dell'indice             *
      *                      *-----------------------------------------*
           move      w-det-pps-zpx-inx    to   w-det-pps-zpx-ies      .
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione                    *
      *                      *-----------------------------------------*
           go to     det-pps-zpx-600.
       det-pps-zpx-700.
      *                  *---------------------------------------------*
      *                  * Test su indice determinato                  *
      *                  *---------------------------------------------*
           if        w-det-pps-zpx-ies    =    zero
                     go to det-pps-zpx-800.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento buono              *
      *                  *---------------------------------------------*
           move      rf-zpx-spi-pvg
                    (w-det-pps-zpx-ies, 1)
                                          to   w-det-pps-zpx-per      .
       det-pps-zpx-800.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-pps-zpx-900.
       det-pps-zpx-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-pps-zpx-999.
       det-pps-zpx-999.
           exit.

      *    *===========================================================*
      *    * Eventuale abbattimento della provvigione in riga          *
      *    *                                                           *
      *    * Input  : w-det-psr-snx-det = Si/No provvigioni in riga    *
      *    *                              determinate                  *
      *    *                                                           *
      *    *          w-det-psr-ibl-pvg = Imponibile determinato       *
      *    *                                                           *
      *    *          w-det-psr-amm-pvg = Ammontare provvigione deter- *
      *    *                              minato                       *
      *    *                                                           *
      *    *          w-det-psr-per-pvg = % provvigione determinata    *
      *    *                                                           *
      *    *          prima dell'eventuale esecuzione dell'abbatti-    *
      *    *          mento                                            *
      *    *                                                           *
      *    * Output : w-det-psr-snx-det                                *
      *    *          w-det-psr-ibl-pvg                                *
      *    *          w-det-psr-amm-pvg                                *
      *    *          w-det-psr-per-pvg                                *
      *    *                                                           *
      *    *          dopo l'eventuale esecuzione dell'abbattimento    *
      *    *-----------------------------------------------------------*
       det-abt-rig-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare a zero per:         *
      *              * - Percentuale unica di sconto determinata       *
      *              * - Percentuale unica di maggiorazione determina- *
      *              *   ta                                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-abt-rig-pus      .
           move      zero                 to   w-det-abt-rig-pum      .
       det-abt-rig-010.
      *              *-------------------------------------------------*
      *              * Se non e' stata determinata alcuna provvigione  *
      *              * in riga : ad uscita senza alcuna azione di ab-  *
      *              * battimento                                      *
      *              *-------------------------------------------------*
           if        d-pvg-age-per-pvg (1)
                                          =    zero and
                     d-pvg-age-per-pvg (2)
                                          =    zero and
                     d-pvg-age-per-pvg (3)
                                          =    zero
                     go to det-abt-rig-900.
       det-abt-rig-030.
      *              *-------------------------------------------------*
      *              * Test ulteriori su personalizzazione             *
      *              *-------------------------------------------------*
           if        w-prs-abr-mab-sco    =    000 and
                     w-prs-abr-mau-mag    =    000
                     go to det-abt-rig-900.
       det-abt-rig-100.
      *              *-------------------------------------------------*
      *              * Determinazione della percentuale di sconto o    *
      *              * di maggiorazione effettiva in riga a seconda    *
      *              * della modalita' di determinazione sconto        *
      *              *                                                 *
      *              * Deviazione ad abbattimento o aumento a seconda  *
      *              * se sconto o maggiorazione                       *
      *              *-------------------------------------------------*
       det-abt-rig-105.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della modalita' di de- *
      *                  * terminazione della percentuale di sconto o  *
      *                  * maggiorazione                               *
      *                  *---------------------------------------------*
           if        w-prs-abr-det-pse    =    01
                     go to det-abt-rig-120
           else if   w-prs-abr-det-pse    =    02
                     go to det-abt-rig-140
           else if   w-prs-abr-det-pse    =    03
                     go to det-abt-rig-160
           else      go to det-abt-rig-180.
       det-abt-rig-120.
      *                  *---------------------------------------------*
      *                  * Se modalita' di determinazione della per-   *
      *                  * tuale di sconto o maggiorazione 01, cioe'   *
      *                  * desunta dalla proporzione tra prezzo lordo  *
      *                  * standard e prezzo netto effettivamente      *
      *                  * praticato                                   *
      *                  *---------------------------------------------*
       det-abt-rig-122.
      *                      *-----------------------------------------*
      *                      * Se non e' memorizzato un prezzo lordo   *
      *                      * standard : ad uscita senza alcuna a-    *
      *                      * zione di abbattimento                   *
      *                      *-----------------------------------------*
           if        d-pvg-age-prz-lrs    =    zero
                     go to det-abt-rig-900.
      *                      *-----------------------------------------*
      *                      * Se non e' memorizzato un prezzo netto   *
      *                      * effettivamente praticato : ad uscita    *
      *                      * senza alcuna azione di abbattimento     *
      *                      *-----------------------------------------*
           if        d-pvg-age-prz-net    =    zero
                     go to det-abt-rig-900.
      *                      *-----------------------------------------*
      *                      * Determinazione della differenza prezzo  *
      *                      *-----------------------------------------*
           subtract  d-pvg-age-prz-net    from d-pvg-age-prz-lrs
                                        giving w-det-abt-rig-dpf      .
      *                      *-----------------------------------------*
      *                      * Se la differenza prezzo e' zero si esce *
      *                      * senza alcuna azione di abbattimento,    *
      *                      * altrimenti si devia a seconda se sconto *
      *                      * o maggiorazione                         *
      *                      *-----------------------------------------*
           if        w-det-abt-rig-dpf    >    zero
                     go to det-abt-rig-124
           else if   w-det-abt-rig-dpf    <    zero
                     go to det-abt-rig-126
           else      go to det-abt-rig-900.
       det-abt-rig-124.
      *                      *-----------------------------------------*
      *                      * Se differenza prezzo per sconto         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione della percentuale u- *
      *                          * nica di sconto                      *
      *                          *-------------------------------------*
           move      d-pvg-age-per-scr (1)
                                          to   w-det-per-uni-001      .
           move      d-pvg-age-per-scr (2)
                                          to   w-det-per-uni-002      .
           move      d-pvg-age-per-scr (3)
                                          to   w-det-per-uni-003      .
           move      d-pvg-age-per-scr (4)
                                          to   w-det-per-uni-004      .
           move      d-pvg-age-per-scr (5)
                                          to   w-det-per-uni-005      .
           perform   det-per-uni-000      thru det-per-uni-999        .
           move      w-det-per-uni-per    to   w-det-abt-rig-pus      .
      *                          *-------------------------------------*
      *                          * Ad abbattimento per sconto          *
      *                          *-------------------------------------*
           go to     det-abt-rig-300.
       det-abt-rig-126.
      *                      *-----------------------------------------*
      *                      * Se differenza prezzo per maggiorazione  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ad aumento per maggiorazione        *
      *                          *-------------------------------------*
           go to     det-abt-rig-600.
       det-abt-rig-140.
      *                  *---------------------------------------------*
      *                  * Se modalita' di determinazione della per-   *
      *                  * tuale di sconto o maggiorazione 02, cioe'   *
      *                  * desunta dalla proporzione tra prezzo netto  *
      *                  * standard e prezzo netto effettivamente      *
      *                  * praticato                                   *
      *                  *---------------------------------------------*
       det-abt-rig-142.
      *                      *-----------------------------------------*
      *                      * Se non e' memorizzato un prezzo netto   *
      *                      * standard : ad uscita senza alcuna a-    *
      *                      * zione di abbattimento                   *
      *                      *-----------------------------------------*
           if        d-pvg-age-prz-nts    =    zero
                     go to det-abt-rig-900.
      *                      *-----------------------------------------*
      *                      * Se non e' memorizzato un prezzo netto   *
      *                      * effettivamente praticato : ad uscita    *
      *                      * senza alcuna azione di abbattimento     *
      *                      *-----------------------------------------*
           if        d-pvg-age-prz-net    =    zero
                     go to det-abt-rig-900.
      *                      *-----------------------------------------*
      *                      * Determinazione della differenza prezzo  *
      *                      *-----------------------------------------*
           subtract  d-pvg-age-prz-net    from d-pvg-age-prz-nts
                                        giving w-det-abt-rig-dpf      .
      *                      *-----------------------------------------*
      *                      * Se la differenza prezzo e' zero si esce *
      *                      * senza alcuna azione di abbattimento,    *
      *                      * altrimenti si devia a seconda se sconto *
      *                      * o maggiorazione                         *
      *                      *-----------------------------------------*
           if        w-det-abt-rig-dpf    >    zero
                     go to det-abt-rig-144
           else if   w-det-abt-rig-dpf    <    zero
                     go to det-abt-rig-146
           else      go to det-abt-rig-900.
       det-abt-rig-144.
      *                      *-----------------------------------------*
      *                      * Se differenza prezzo per sconto         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione della percentuale u- *
      *                          * nica di sconto                      *
      *                          *-------------------------------------*
           move      d-pvg-age-per-scr (1)
                                          to   w-det-per-uni-001      .
           move      d-pvg-age-per-scr (2)
                                          to   w-det-per-uni-002      .
           move      d-pvg-age-per-scr (3)
                                          to   w-det-per-uni-003      .
           move      d-pvg-age-per-scr (4)
                                          to   w-det-per-uni-004      .
           move      d-pvg-age-per-scr (5)
                                          to   w-det-per-uni-005      .
           perform   det-per-uni-000      thru det-per-uni-999        .
           move      w-det-per-uni-per    to   w-det-abt-rig-pus      .
      *                          *-------------------------------------*
      *                          * Ad abbattimento per sconto          *
      *                          *-------------------------------------*
           go to     det-abt-rig-300.
       det-abt-rig-146.
      *                      *-----------------------------------------*
      *                      * Se differenza prezzo per maggiorazione  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ad aumento per maggiorazione        *
      *                          *-------------------------------------*
           go to     det-abt-rig-600.
       det-abt-rig-160.
      *                  *---------------------------------------------*
      *                  * Se modalita' di determinazione della per-   *
      *                  * tuale di sconto o maggiorazione 03, cioe'   *
      *                  * desunta dalla sommatoria scalare dei max 5  *
      *                  * sconti effettivamente praticati, senza te-  *
      *                  * ner conto del prezzo                        *
      *                  *---------------------------------------------*
       det-abt-rig-162.
      *                      *-----------------------------------------*
      *                      * Determinazione di una percentuale di    *
      *                      * sconto come sommatoria delle cinque     *
      *                      * percentuali di sconto in riga           *
      *                      *-----------------------------------------*
           move      d-pvg-age-per-scr (1)
                                          to   w-det-per-uni-001      .
           move      d-pvg-age-per-scr (2)
                                          to   w-det-per-uni-002      .
           move      d-pvg-age-per-scr (3)
                                          to   w-det-per-uni-003      .
           move      d-pvg-age-per-scr (4)
                                          to   w-det-per-uni-004      .
           move      d-pvg-age-per-scr (5)
                                          to   w-det-per-uni-005      .
           perform   det-per-uni-000      thru det-per-uni-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione della percentuale unica  *
      *                      * di sconto determinata                   *
      *                      *-----------------------------------------*
           move      w-det-per-uni-per    to   w-det-abt-rig-pus      .
      *                      *-----------------------------------------*
      *                      * Ad abbattimento per sconto              *
      *                      *-----------------------------------------*
           go to     det-abt-rig-300.
       det-abt-rig-180.
      *                  *---------------------------------------------*
      *                  * Se modalita' di determinazione della per-   *
      *                  * tuale di sconto o maggiorazione : codice    *
      *                  * non riconosciuto                            *
      *                  *---------------------------------------------*
       det-abt-rig-182.
      *                      *-----------------------------------------*
      *                      * Ad uscita senza alcuna azione di abbat- *
      *                      * timento                                 *
      *                      *-----------------------------------------*
           go to     det-abt-rig-900.
       det-abt-rig-300.
      *              *-------------------------------------------------*
      *              * Abbattimento per sconto                         *
      *              *-------------------------------------------------*
       det-abt-rig-310.
      *                  *---------------------------------------------*
      *                  * Se non c'e' alcun abbattimento da eseguire  *
      *                  * in caso di sconto : ad uscita senza alcuna  *
      *                  * azione di abbattimento                      *
      *                  *---------------------------------------------*
           if        w-prs-abr-mab-sco    =    000
                     move  zero           to   w-det-abt-rig-pus
                     go to det-abt-rig-900.
       det-abt-rig-320.
      *                  *---------------------------------------------*
      *                  * Se la percentuale unica di sconto determi-  *
      *                  * nata e' a zero : ad uscita senza alcuna     *
      *                  * azione di abbattimento                      *
      *                  *---------------------------------------------*
           if        w-det-abt-rig-pus    =    zero
                     go to det-abt-rig-900.
       det-abt-rig-325.
      *                  *---------------------------------------------*
      *                  * Se la percentuale unica di sconto determi-  *
      *                  * nata non e' superiore a quella massima :    *
      *                  * ad uscita senza alcuna azione di abbatti-   *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           if        w-prs-abr-max-psc    =    zero
                     go to det-abt-rig-330.
           if        w-det-abt-rig-pus    <    w-prs-abr-max-psc
                     move  zero           to   w-det-abt-rig-pus
                     go to det-abt-rig-900.
       det-abt-rig-330.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di abbatti-   *
      *                  * mento da eseguire                           *
      *                  *---------------------------------------------*
           if        w-prs-abr-mab-sco    =    100
                     go to det-abt-rig-340
           else if   w-prs-abr-mab-sco    >    100 and
                     w-prs-abr-mab-sco    <    200
                     go to det-abt-rig-360
           else      go to det-abt-rig-380.
       det-abt-rig-340.
      *                  *---------------------------------------------*
      *                  * Se tipo di abbattimento 100                 *
      *                  *                                             *
      *                  * Alla percentuale di provvigione normale     *
      *                  * viene sottratta la percentuale di scon-     *
      *                  * to effettiva                                *
      *                  *---------------------------------------------*
       det-abt-rig-345.
      *                      *-----------------------------------------*
      *                      * Se la percentuale di sconto supera la   *
      *                      * percentuale di provvigione si normaliz- *
      *                      * zano i valori in uscita e si esce       *
      *                      *-----------------------------------------*
           if        w-det-abt-rig-pus    not  < w-det-psr-per-pvg
                     move  "N"            to   w-det-psr-snx-det
                     move  zero           to   w-det-psr-ibl-pvg
                     move  zero           to   w-det-psr-amm-pvg
                     move  zero           to   w-det-psr-per-pvg
                     move  zero           to   w-det-abt-rig-pus
                     go to det-abt-rig-900.
       det-abt-rig-350.
      *                      *-----------------------------------------*
      *                      * Determinazione percentuale di provvi-   *
      *                      * gione abbattuta                         *
      *                      *-----------------------------------------*
           subtract  w-det-abt-rig-pus    from w-det-psr-per-pvg      .
       det-abt-rig-352.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-abt-rig-900.
       det-abt-rig-360.
      *                  *---------------------------------------------*
      *                  * Se tipo di abbattimento 101.199             *
      *                  *                                             *
      *                  * Alla percentuale di provvigione normale     *
      *                  * viene sottratto l'nn% della percentuale     *
      *                  * di sconto effettiva                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione % di abbattimento        *
      *                      *-----------------------------------------*
           subtract  100                  from w-prs-abr-mab-sco
                                        giving w-det-abt-rig-pda      .
      *                          *-------------------------------------*
      *                          * Test se presente il decimale facol- *
      *                          * tativo                              *
      *                          *-------------------------------------*
           if        w-prs-abr-mab-scd    =    zero
                     go to det-abt-rig-363.
           move      w-prs-abr-mab-scd    to   w-det-abt-rig-pwa      .
           divide    10                   into w-det-abt-rig-pwa      .
           add       w-det-abt-rig-pwa    to   w-det-abt-rig-pda      .
       det-abt-rig-363.
      *                      *-----------------------------------------*
      *                      * Determinazione % di sconto abbattuta    *
      *                      *-----------------------------------------*
           move      w-det-abt-rig-pus    to   w-det-abt-rig-psa      .
           multiply  w-det-abt-rig-pda    by   w-det-abt-rig-psa      .
           divide    100                  into w-det-abt-rig-psa      .
       det-abt-rig-365.
      *                      *-----------------------------------------*
      *                      * Se la percentuale di sconto abbattuta   *
      *                      * supera la percentuale di provvigione si *
      *                      * normalizzano i valori in uscita e si e- *
      *                      * sce                                     *
      *                      *-----------------------------------------*
           if        w-det-abt-rig-psa    not  < w-det-psr-per-pvg
                     move  "N"            to   w-det-psr-snx-det
                     move  zero           to   w-det-psr-ibl-pvg
                     move  zero           to   w-det-psr-amm-pvg
                     move  zero           to   w-det-psr-per-pvg
                     move  zero           to   w-det-abt-rig-pus
                     go to det-abt-rig-900.
       det-abt-rig-370.
      *                      *-----------------------------------------*
      *                      * Determinazione della percentuale di     *
      *                      * provvigione abbattuta                   *
      *                      *-----------------------------------------*
           subtract  w-det-abt-rig-psa    from w-det-psr-per-pvg      .
       det-abt-rig-375.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-abt-rig-900.
       det-abt-rig-380.
      *                  *---------------------------------------------*
      *                  * Se tipo di abbattimento di tipo non ricono- *
      *                  * sciuto                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Percentuale unica di sconto a zero      *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-abt-rig-pus      .
      *                      *-----------------------------------------*
      *                      * Ad uscita senza alcuna azione di abbat- *
      *                      * timento                                 *
      *                      *-----------------------------------------*
           go to     det-abt-rig-900.
       det-abt-rig-600.
      *              *=================================================*
      *              * Aumento per maggiorazione                       *
      *              *-------------------------------------------------*
       det-abt-rig-610.
      *                  *---------------------------------------------*
      *                  * Se non c'e' alcun aumento da eseguiire in   *
      *                  * caso di maggiorazione : ad uscita senza al- *
      *                  * cuna azione di abbattimento                 *
      *                  *---------------------------------------------*
           if        w-prs-abr-mau-mag    =    000
                     move  zero           to   w-det-abt-rig-pum
                     go to det-abt-rig-900.
       det-abt-rig-620.
      *                  *---------------------------------------------*
      *                  * Se la percentuale unica di maggiorazione    *
      *                  * determinata e' a zero : ad uscita senza     *
      *                  * alcuna azione di abbattimento/aumento       *
      *                  *---------------------------------------------*
           if        w-det-abt-rig-pum    =    zero
                     go to det-abt-rig-900.
       det-abt-rig-630.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di aumento    *
      *                  * da eseguire                                 *
      *                  *---------------------------------------------*
           if        w-prs-abr-mau-mag    =    100
                     go to det-abt-rig-640
           else if   w-prs-abr-mau-mag    >    100 and
                     w-prs-abr-mau-mag    <    200
                     go to det-abt-rig-660
           else      go to det-abt-rig-680.
       det-abt-rig-640.
      *                  *---------------------------------------------*
      *                  * Se tipo di aumento 100                      *
      *                  *                                             *
      *                  * Alla percentuale di provvigione normale     *
      *                  * viene aggiunta la percentuale di maggio-    *
      *                  * razione effettiva                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione della percentuale di     *
      *                      * provvigione aumentata                   *
      *                      *-----------------------------------------*
           move      w-det-abt-rig-pum    to   w-det-abt-rig-psa      .
           add       w-det-psr-per-pvg    to   w-det-abt-rig-psa      .
      *                      *-----------------------------------------*
      *                      * Se maggiore del massimo consentito la   *
      *                      * si riduce al massimo                    *
      *                      *-----------------------------------------*
           if        w-det-abt-rig-psa    >    w-prs-abr-max-psa
                     move  w-prs-abr-max-psa
                                          to   w-det-abt-rig-psa      .
      *                      *-----------------------------------------*
      *                      * Se la percentuale di maggiorazione au-  *
      *                      * mentata e' a zero si normalizzano i va- *
      *                      * lori in uscita e si esce                *
      *                      *-----------------------------------------*
           if        w-det-abt-rig-psa    not  >    zero
                     move  "N"            to   w-det-psr-snx-det
                     move  zero           to   w-det-psr-ibl-pvg
                     move  zero           to   w-det-psr-amm-pvg
                     move  zero           to   w-det-psr-per-pvg
                     move  zero           to   w-det-abt-rig-pum
                     go to det-abt-rig-900.
      *                      *-----------------------------------------*
      *                      * Memorizzazione della percentuale di     *
      *                      * provvigione aumentata                   *
      *                      *-----------------------------------------*
           move      w-det-abt-rig-psa    to   w-det-psr-per-pvg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-abt-rig-900.
       det-abt-rig-660.
      *                  *---------------------------------------------*
      *                  * Se tipo di aumento 101.199                  *
      *                  *                                             *
      *                  * Alla percentuale di provvigione normale     *
      *                  * viene aggiunto l'nn% del percentuale di     *
      *                  * maggiorazione effettiva                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione % di aumento             *
      *                      *-----------------------------------------*
           subtract  100                  from w-prs-abr-mau-mag
                                        giving w-det-abt-rig-pda      .
           subtract  w-det-abt-rig-pda    from 100
                                        giving w-det-abt-rig-pda      .
      *                      *-----------------------------------------*
      *                      * Determinazione % di provvigione aumen-  *
      *                      * tata                                    *
      *                      *-----------------------------------------*
           move      w-det-abt-rig-pum    to   w-det-abt-rig-psa      .
           multiply  w-det-abt-rig-pda    by   w-det-abt-rig-psa      .
           divide    100                  into w-det-abt-rig-psa      .
           add       w-det-psr-per-pvg    to   w-det-abt-rig-psa      .
      *                      *-----------------------------------------*
      *                      * Se maggiore del massimo consentito la   *
      *                      * si riduce al massimo                    *
      *                      *-----------------------------------------*
           if        w-det-abt-rig-psa    >    w-prs-abr-max-psa
                     move  w-prs-abr-max-psa
                                          to   w-det-abt-rig-psa      .
      *                      *-----------------------------------------*
      *                      * Se la percentuale di maggiorazione au-  *
      *                      * mentata e' a zero si normalizzano i va- *
      *                      * lori in uscita e si esce                *
      *                      *-----------------------------------------*
           if        w-det-abt-rig-psa    not  >    zero
                     move  "N"            to   w-det-psr-snx-det
                     move  zero           to   w-det-psr-ibl-pvg
                     move  zero           to   w-det-psr-amm-pvg
                     move  zero           to   w-det-psr-per-pvg
                     move  zero           to   w-det-abt-rig-pum
                     go to det-abt-rig-900.
      *                      *-----------------------------------------*
      *                      * Memorizzazione della percentuale di     *
      *                      * provvigione aumentata                   *
      *                      *-----------------------------------------*
           move      w-det-abt-rig-psa    to   w-det-psr-per-pvg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-abt-rig-900.
       det-abt-rig-680.
      *                  *---------------------------------------------*
      *                  * Se tipo di aumento di tipo non ricono-      *
      *                  * sciuto                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Percentuale unica di maggiorazione a    *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-abt-rig-pum      .
      *                      *-----------------------------------------*
      *                      * Ad uscita senza alcuna azione di abbat- *
      *                      * timento                                 *
      *                      *-----------------------------------------*
           go to     det-abt-rig-900.
       det-abt-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-abt-rig-999.
       det-abt-rig-999.
           exit.

