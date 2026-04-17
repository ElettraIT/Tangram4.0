       Identification Division.
       Program-Id.                                 pmag300y           .
      *================================================================*
      *                                                                *
      * Modulo per la determinazione saldi gestione magazzino          *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * PROMEMORIA: bisogna rivedere le giacenze per dislocazione      *
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
      *        Input  : d-sld-mag-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-sld-mag-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-sld-mag-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-sld-mag-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "SL" - Determinazione saldo di magazzino ad una certa data     *
      *                                                                *
      *                                                                *
      *        Input  :                                                *
      *                                                                *
      *              Parametri validi per ogni valore di 'tip-sld'     *
      *                                                                *
      *                 d-sld-mag-tip-ope = "SL"                       *
      *                                                                *
      *                 d-sld-mag-tip-sld = Tipo di saldo              *
      *                                     0000 : Giacenza fisica     *
      *                                     0100 : Giacenza di pro-    *
      *                                            prieta'             *
      *                                     0200 : Giacenza merce      *
      *                                            presso terzi ma non *
      *                                            di loro proprieta'  *
      *                                     0300 : Giacenza merce      *
      *                                            presso di noi ma    *
      *                                            non di nostra pro-  *
      *                                            prieta'             *
      *                                                                *
      *                 d-sld-mag-dat-sld = Data di riferimento (se    *
      *                                     zero si riferisce alla da- *
      *                                     ta attuale                 *
      *                                                                *
      *                 d-sld-mag-uot-dpz = Una o tutte le dipendenze  *
      *                                     U : Una dipendenza         *
      *                                     T : Tutte le dipendenze    *
      *                                                                *
      *                 d-sld-mag-cod-dpz = Codice dipendenza          *
      *                                     (obbligatorio se campo     *
      *                                     precedente vale U)         *
      *                                                                *
      *                 d-sld-mag-tip-mag = Tipo codice di magazzino   *
      *                                     (obbligatorio)             *
      *                                                                *
      *                 d-sld-mag-num-mag = Codice numerico di magaz-  *
      *                                     zino (obbligatorio)        *
      *                                                                *
      *              Altri parametri per tip-sld = 0000                *
      *                                                                *
      *                 d-sld-mag-uot-var = Una o tutte le varianti    *
      *                                     U : Una variante           *
      *                                     T : Tutte le varianti      *
      *                                                                *
      *                 d-sld-mag-var-mag = Codice variante            *
      *                                     (obbligatorio se campo     *
      *                                     precedente vale U; puo'    *
      *                                     essere anche a spazi)      *
      *                                                                *
      *                 d-sld-mag-uot-dsl = Una o tutte dislocazioni   *
      *                                     U : Una dislocazione       *
      *                                     T : Tutte le dislocazioni  *
      *                                                                *
      *                 d-sld-mag-cod-dsl = Codice dislocazione        *
      *                                     (obbligatorio se campo     *
      *                                     precedente vale U; puo'    *
      *                                     essere anche a spazi)      *
      *                                                                *
      *              Altri parametri per tip-sld = 0100                *
      *                                                                *
      *                 d-sld-mag-uot-var = Una o tutte le varianti    *
      *                                     U : Una variante           *
      *                                     T : Tutte le varianti      *
      *                                                                *
      *                 d-sld-mag-var-mag = Codice variante            *
      *                                     (obbligatorio se campo     *
      *                                     precedente vale U; puo'    *
      *                                     essere anche a spazi)      *
      *                                                                *
      *              Altri parametri per tip-sld = 0200 o 0300         *
      *                                                                *
      *                 d-sld-mag-uot-var = Una o tutte le varianti    *
      *                                     U : Una variante           *
      *                                     T : Tutte le varianti      *
      *                                                                *
      *                 d-sld-mag-var-mag = Codice variante            *
      *                                     (obbligatorio se campo     *
      *                                     precedente vale U; puo'    *
      *                                     essere anche a spazi)      *
      *                                                                *
      *                 d-sld-mag-uot-ctm = Una o tutti i conti merce  *
      *                                     U : Un conto merce         *
      *                                     T : Tutti i conti merce    *
      *                                                                *
      *                 d-sld-mag-cod-ctm = Codice conto merce         *
      *                                     (obbligatorio se campo     *
      *                                     precedente vale U; puo'    *
      *                                     essere anche a spazi)      *
      *                                                                *
      *                 d-sld-mag-tip-arc = Tipo archivio              *
      *                                     Spaces : Tutti             *
      *                                     C      : Clienti           *
      *                                     F      : Fornitori         *
      *                                     A      : Agenti            *
      *                                                                *
      *                 d-sld-mag-uot-arc = Uno o tutti i codici ar-   *
      *                                     chivio                     *
      *                                     U : Un codice archivio     *
      *                                     T : Tutti i codici archi-  *
      *                                         vio                    *
      *                                                                *
      *                 d-sld-mag-cod-arc = Codice archivio            *
      *                                     (obbligatorio se campo     *
      *                                     precedente vale U; puo'    *
      *                                     essere anche a zero)       *
      *                                                                *
      *                 d-sld-mag-uot-dpa = Una o tutte le dipendenze  *
      *                                     del codice archivio        *
      *                                     U : Una sola dipendenza    *
      *                                         del codice archivio    *
      *                                     T : Tutte le dipendenze    *
      *                                         del codice archivio    *
      *                                     (obbligatorio se un solo   *
      *                                     codice archivio)           *
      *                                                                *
      *                 d-sld-mag-dpz-arc = Codice archivio            *
      *                                     (obbligatorio se campo     *
      *                                     precedente vale U; puo'    *
      *                                     essere anche a spaces)     *
      *                                                                *
      *                                                                *
      *        Output :                                                *
      *                                                                *
      *              Valori validi per ogni valore di 'tip-sld'        *
      *                                                                *
      *                 d-sld-mag-exi-sts = Esito dell'operazione      *
      *                                      - Spaces : Ok             *
      *                                      - #      : Errore di ese- *
      *                                                 cuzione        *
      *                                                                *
      *                 d-sld-mag-msg-exi = Messaggio di errore, solo  *
      *                                     se errore di esecuzione    *
      *                                                                *
      *              Altri valori per tip-sld = 0000                   *
      *                                                                *
      *                 d-sld-mag-sld-mag = Giacenza fisica alla data  *
      *                                     richiesta                  *
      *                                                                *
      *                 d-sld-mag-sub-s01 = Giacenza fisica inizio e-  *
      *                                     sercizio                   *
      *                                                                *
      *                 d-sld-mag-sub-s02 = Totale carichi esercizio   *
      *                                                                *
      *                 d-sld-mag-sub-s03 = Totale scarichi esercizio  *
      *                                                                *
      *              Altri valori per tip-sld = 0100                   *
      *                                                                *
      *                 d-sld-mag-sld-mag = Giacenza di proprieta' al- *
      *                                     la data richiesta          *
      *                                                                *
      *                 d-sld-mag-sub-s01 = Giacenza fisica alla data  *
      *                                     richiesta                  *
      *                                                                *
      *                 d-sld-mag-sub-s02 = Giacenza presso terzi ma   *
      *                                     non di loro proprieta'     *
      *                                     alla data richiesta        *
      *                                                                *
      *                 d-sld-mag-sub-s03 = Giacenza presso di noi ma  *
      *                                     non di nostra proprieta'   *
      *                                     alla data richiesta        *
      *                                                                *
      *              Altri valori per tip-sld = 0200                   *
      *                                                                *
      *                 d-sld-mag-sld-mag = Giacenza presso terzi ma   *
      *                                     non di loro proprieta'     *
      *                                     alla data richiesta        *
      *                                                                *
      *                 d-sld-mag-sub-s01 = Giacenza presso terzi ma   *
      *                                     non di loro proprieta'     *
      *                                     ad inizio esercizio        *
      *                                                                *
      *                 d-sld-mag-sub-s02 = Progressivo movimenti a    *
      *                                     credito da inizio eserci-  *
      *                                     zio fino alla data ri-     *
      *                                     chiesta                    *
      *                                                                *
      *                 d-sld-mag-sub-s03 = Progressivo movimenti a    *
      *                                     debito da inizio eserci-   *
      *                                     zio fino alla data ri-     *
      *                                     chiesta                    *
      *                                                                *
      *              Altri valori per tip-sld = 0300                   *
      *                                                                *
      *                 d-sld-mag-sld-mag = Giacenza presso di noi ma  *
      *                                     non di nostra proprieta'   *
      *                                     alla data richiesta        *
      *                                                                *
      *                 d-sld-mag-sub-s01 = Giacenza presso di noi ma  *
      *                                     non di nostra proprieta'   *
      *                                     ad inizio esercizio        *
      *                                                                *
      *                 d-sld-mag-sub-s02 = Progressivo movimenti a    *
      *                                     credito da inizio eserci-  *
      *                                     zio fino alla data ri-     *
      *                                     chiesta                    *
      *                                                                *
      *                 d-sld-mag-sub-s03 = Progressivo movimenti a    *
      *                                     debito da inizio eserci-   *
      *                                     zio fino alla data ri-     *
      *                                     chiesta                    *
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
      *        * [mms]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmms"                          .
      *        *-------------------------------------------------------*
      *        * [mmz]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmz"                          .
      *        *-------------------------------------------------------*
      *        * [mmr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmr"                          .
      *        *-------------------------------------------------------*
      *        * [datife]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/num/rec/rndatife"                       .

      *    *===========================================================*
      *    * Work-area date e numerazioni                              *
      *    *-----------------------------------------------------------*
       01  w-dnu.
      *        *-------------------------------------------------------*
      *        * Da record numerazioni [datife]                        *
      *        *-------------------------------------------------------*
           05  w-dnu-ife.
      *            *---------------------------------------------------*
      *            * Flag di avanzamento inventario fine esercizio     *
      *            * - Spaces : Completamente eseguito                 *
      *            * - #      : In progresso                           *
      *            *---------------------------------------------------*
               10  w-dnu-ife-ife-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Data apertura periodo inventario fine esercizio   *
      *            *---------------------------------------------------*
               10  w-dnu-ife-ife-dti      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data chiusura periodo inventario fine esercizio   *
      *            *---------------------------------------------------*
               10  w-dnu-ife-ife-dtf      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Anno di esercizio dell'inventario di fine eser-   *
      *            * cizio : s.aa                                      *
      *            *---------------------------------------------------*
               10  w-dnu-ife-ife-ese      pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per determinazioni generali, valide per tutto il *
      *        * programma                                             *
      *        *-------------------------------------------------------*
           05  w-det-prg.
      *            *---------------------------------------------------*
      *            * Mese di chiusura esercizio, determinato alla fun- *
      *            * zione Open                                        *
      *            *---------------------------------------------------*
               10  w-det-prg-mes-che      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazioni saldi, valide per tutti i ti- *
      *        * pi di determinazione                                  *
      *        *-------------------------------------------------------*
           05  w-det-dsd.
      *            *---------------------------------------------------*
      *            * Data attuale                                      *
      *            *---------------------------------------------------*
               10  w-det-dsd-dat-att      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data di riferimento per determinazione saldi      *
      *            *---------------------------------------------------*
               10  w-det-dsd-dat-rif      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Flag indicatore se la data di riferimento e' un   *
      *            * fine mese                                         *
      *            *   - Spaces : No                                   *
      *            *   - F      : Si                                   *
      *            *---------------------------------------------------*
               10  w-det-dsd-fin-mes      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Indice su mese di esercizio relativo alla data di *
      *            * riferimento per determinazione saldi              *
      *            *---------------------------------------------------*
               10  w-det-dsd-ism-rif      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Secondo anno di esercizio da considerare          *
      *            *---------------------------------------------------*
               10  w-det-dsd-2do-ese      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Primo anno di esercizio da considerare            *
      *            *---------------------------------------------------*
               10  w-det-dsd-1mo-ese      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Flag indicatore se necessario esplorare uno o due *
      *            * esercizi                                          *
      *            *   - 1 : 1 esercizio                               *
      *            *   - 2 : 2 esercizi                                *
      *            *---------------------------------------------------*
               10  w-det-dsd-1o2-ese      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione saldi da [mms]                *
      *        *-------------------------------------------------------*
           05  w-det-mms.
      *            *---------------------------------------------------*
      *            * Input                                             *
      *            *---------------------------------------------------*
               10  w-det-mms-inp.
      *                *-----------------------------------------------*
      *                * Data attuale                                  *
      *                *-----------------------------------------------*
                   15  w-det-mms-dat-att  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Data di riferimento per determinazione saldi  *
      *                *-----------------------------------------------*
                   15  w-det-mms-dat-rif  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Flag indicatore se la data di riferimento e'  *
      *                * un fine mese                                  *
      *                *   - Spaces : No                               *
      *                *   - F      : Si                               *
      *                *-----------------------------------------------*
                   15  w-det-mms-fin-mes  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Indice su mese di esercizio relativo alla da- *
      *                * ta di riferimento per determinazione saldi    *
      *                *-----------------------------------------------*
                   15  w-det-mms-ism-rif  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Flag indicatore se necessario esplorare uno   *
      *                * o due esercizi                                *
      *                *   - 1 : 1 esercizio                           *
      *                *   - 2 : 2 esercizi                            *
      *                *-----------------------------------------------*
                   15  w-det-mms-1o2-ese  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Primo anno di esercizio da considerare        *
      *                *-----------------------------------------------*
                   15  w-det-mms-1mo-ese  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Secondo anno di esercizio da considerare      *
      *                *-----------------------------------------------*
                   15  w-det-mms-2do-ese  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Tipo codice di magazzino                      *
      *                *-----------------------------------------------*
                   15  w-det-mms-tip-mag  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico di magazzino                  *
      *                *-----------------------------------------------*
                   15  w-det-mms-num-mag  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Una o tutte le dipendenze                     *
      *                *-----------------------------------------------*
                   15  w-det-mms-uot-dpz  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza                             *
      *                *-----------------------------------------------*
                   15  w-det-mms-cod-dpz  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Una o tutte le varianti                       *
      *                *-----------------------------------------------*
                   15  w-det-mms-uot-var  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice variante                               *
      *                *-----------------------------------------------*
                   15  w-det-mms-var-mag  pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Una o tutte le dislocazioni                   *
      *                *-----------------------------------------------*
                   15  w-det-mms-uot-dsl  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice dislocazione                           *
      *                *-----------------------------------------------*
                   15  w-det-mms-cod-dsl  pic  x(07)                  .
      *            *---------------------------------------------------*
      *            * Output                                            *
      *            *---------------------------------------------------*
               10  w-det-mms-out.
      *                *-----------------------------------------------*
      *                * 1. esercizio   : Giacenza fisica inizio anno  *
      *                *-----------------------------------------------*
                   15  w-det-mms-1oe-gfi      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 1. esercizio   : Totale carichi anno          *
      *                *-----------------------------------------------*
                   15  w-det-mms-1oe-tca      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 1. esercizio   : Totale scarichi anno         *
      *                *-----------------------------------------------*
                   15  w-det-mms-1oe-tsa      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 2. esercizio   : Giacenza fisica inizio anno  *
      *                *-----------------------------------------------*
                   15  w-det-mms-2oe-gfi      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 2. esercizio   : Totale carichi periodo       *
      *                *-----------------------------------------------*
                   15  w-det-mms-2oe-tcp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 2. esercizio   : Totale scarichi periodo      *
      *                *-----------------------------------------------*
                   15  w-det-mms-2oe-tsp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Giacenza fisica inizio anno  *
      *                *-----------------------------------------------*
                   15  w-det-mms-det-gfi      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Totale carichi periodo       *
      *                *-----------------------------------------------*
                   15  w-det-mms-det-tcp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Totale scarichi periodo      *
      *                *-----------------------------------------------*
                   15  w-det-mms-det-tsp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Giacenza fisica fine periodo *
      *                *-----------------------------------------------*
                   15  w-det-mms-det-gff      pic s9(08)v9(03)        .
      *            *---------------------------------------------------*
      *            * Sub-work                                          *
      *            *---------------------------------------------------*
               10  w-det-mms-sub.
      *                *-----------------------------------------------*
      *                * Anno di esercizio per subroutines :           *
      *                *   - Start                                     *
      *                *   - Read next                                 *
      *                *   - Test max                                  *
      *                *   - Selezione                                 *
      *                *-----------------------------------------------*
                   15  w-det-mms-sub-ese  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Data relativa al primo giorno del mese della  *
      *                * data di riferimento                           *
      *                *-----------------------------------------------*
                   15  w-det-mms-g1m-ddr  pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Flags                                             *
      *            *---------------------------------------------------*
               10  w-det-mms-flg.
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Start               *
      *                *-----------------------------------------------*
                   15  w-det-mms-str-flg      pic  x(01)              .
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Read Next           *
      *                *-----------------------------------------------*
                   15  w-det-mms-nxt-flg      pic  x(01)              .
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Test Max            *
      *                *-----------------------------------------------*
                   15  w-det-mms-max-flg      pic  x(01)              .
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Selezione           *
      *                *-----------------------------------------------*
                   15  w-det-mms-sel-flg      pic  x(01)              .
      *            *---------------------------------------------------*
      *            * Indici                                            *
      *            *---------------------------------------------------*
               10  w-det-mms-inx.
      *                *-----------------------------------------------*
      *                * Indice su mesi 1..12                          *
      *                *-----------------------------------------------*
                   15  w-det-mms-inx-mes      pic  9(02)              .
      *        *-------------------------------------------------------*
      *        * Work per determinazione saldi da [mmz] per tipo conto *
      *        * merce 'N'                                             *
      *        *-------------------------------------------------------*
           05  w-det-mzn.
      *            *---------------------------------------------------*
      *            * Input                                             *
      *            *---------------------------------------------------*
               10  w-det-mzn-inp.
      *                *-----------------------------------------------*
      *                * Data attuale                                  *
      *                *-----------------------------------------------*
                   15  w-det-mzn-dat-att  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Data di riferimento per determinazione saldi  *
      *                *-----------------------------------------------*
                   15  w-det-mzn-dat-rif  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Flag indicatore se la data di riferimento e'  *
      *                * un fine mese                                  *
      *                *   - Spaces : No                               *
      *                *   - F      : Si                               *
      *                *-----------------------------------------------*
                   15  w-det-mzn-fin-mes  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Indice su mese di esercizio relativo alla da- *
      *                * ta di riferimento per determinazione saldi    *
      *                *-----------------------------------------------*
                   15  w-det-mzn-ism-rif  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Flag indicatore se necessario esplorare uno   *
      *                * o due esercizi                                *
      *                *   - 1 : 1 esercizio                           *
      *                *   - 2 : 2 esercizi                            *
      *                *-----------------------------------------------*
                   15  w-det-mzn-1o2-ese  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Primo anno di esercizio da considerare        *
      *                *-----------------------------------------------*
                   15  w-det-mzn-1mo-ese  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Secondo anno di esercizio da considerare      *
      *                *-----------------------------------------------*
                   15  w-det-mzn-2do-ese  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Tipo codice di magazzino                      *
      *                *-----------------------------------------------*
                   15  w-det-mzn-tip-mag  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico di magazzino                  *
      *                *-----------------------------------------------*
                   15  w-det-mzn-num-mag  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Una o tutte le dipendenze                     *
      *                *-----------------------------------------------*
                   15  w-det-mzn-uot-dpz  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza                             *
      *                *-----------------------------------------------*
                   15  w-det-mzn-cod-dpz  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Una o tutte le varianti                       *
      *                *-----------------------------------------------*
                   15  w-det-mzn-uot-var  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice variante                               *
      *                *-----------------------------------------------*
                   15  w-det-mzn-var-mag  pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Uno o tutti i conti merce                     *
      *                *-----------------------------------------------*
                   15  w-det-mzn-uot-ctm  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice conto merce                            *
      *                *-----------------------------------------------*
                   15  w-det-mzn-cod-ctm  pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Tipo archivio                                 *
      *                *  - Spaces : Tutti                             *
      *                *  - C      : Clienti                           *
      *                *  - F      : Fornitori                         *
      *                *  - A      : Agenti                            *
      *                *-----------------------------------------------*
                   15  w-det-mzn-tip-arc  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Uno o tutti i codici archivio                 *
      *                *-----------------------------------------------*
                   15  w-det-mzn-uot-arc  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice archivio                               *
      *                *-----------------------------------------------*
                   15  w-det-mzn-cod-arc  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Una o tutte le dipendenze per il codice ar-   *
      *                * chivio                                        *
      *                *-----------------------------------------------*
                   15  w-det-mzn-uot-dpa  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza per il codice archivio      *
      *                *-----------------------------------------------*
                   15  w-det-mzn-dpz-arc  pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Output                                            *
      *            *---------------------------------------------------*
               10  w-det-mzn-out.
      *                *-----------------------------------------------*
      *                * 1. esercizio   : Credito inizio anno          *
      *                *-----------------------------------------------*
                   15  w-det-mzn-1oe-cri      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 1. esercizio   : Totale movimenti a nostro    *
      *                *                  credito anno                 *
      *                *-----------------------------------------------*
                   15  w-det-mzn-1oe-tca      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 1. esercizio   : Totale movimenti a nostro    *
      *                *                  debito anno                  *
      *                *-----------------------------------------------*
                   15  w-det-mzn-1oe-tda      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 2. esercizio   : Credito inizio anno          *
      *                *-----------------------------------------------*
                   15  w-det-mzn-2oe-cri      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 2. esercizio   : Totale movimenti a nostro    *
      *                *                  credito periodo              *
      *                *-----------------------------------------------*
                   15  w-det-mzn-2oe-tcp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 2. esercizio   : Totale movimenti a nostro    *
      *                *                  debito periodo               *
      *                *-----------------------------------------------*
                   15  w-det-mzn-2oe-tdp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Credito inizio anno          *
      *                *-----------------------------------------------*
                   15  w-det-mzn-det-cri      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Totale movimenti a nostro    *
      *                *                  credito periodo              *
      *                *-----------------------------------------------*
                   15  w-det-mzn-det-tcp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Totale movimenti a nostro    *
      *                *                  debito periodo               *
      *                *-----------------------------------------------*
                   15  w-det-mzn-det-tdp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Credito fine periodo         *
      *                *-----------------------------------------------*
                   15  w-det-mzn-det-crf      pic s9(08)v9(03)        .
      *            *---------------------------------------------------*
      *            * Sub-work                                          *
      *            *---------------------------------------------------*
               10  w-det-mzn-sub.
      *                *-----------------------------------------------*
      *                * Anno di esercizio per subroutines :           *
      *                *   - Start                                     *
      *                *   - Read next                                 *
      *                *   - Test max                                  *
      *                *   - Selezione                                 *
      *                *-----------------------------------------------*
                   15  w-det-mzn-sub-ese  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Data relativa al primo giorno del mese della  *
      *                * data di riferimento                           *
      *                *-----------------------------------------------*
                   15  w-det-mzn-g1m-ddr  pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Flags                                             *
      *            *---------------------------------------------------*
               10  w-det-mzn-flg.
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Start               *
      *                *-----------------------------------------------*
                   15  w-det-mzn-str-flg      pic  x(01)              .
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Read Next           *
      *                *-----------------------------------------------*
                   15  w-det-mzn-nxt-flg      pic  x(01)              .
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Test Max            *
      *                *-----------------------------------------------*
                   15  w-det-mzn-max-flg      pic  x(01)              .
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Selezione           *
      *                *-----------------------------------------------*
                   15  w-det-mzn-sel-flg      pic  x(01)              .
      *            *---------------------------------------------------*
      *            * Indici                                            *
      *            *---------------------------------------------------*
               10  w-det-mzn-inx.
      *                *-----------------------------------------------*
      *                * Indice su mesi 1..12                          *
      *                *-----------------------------------------------*
                   15  w-det-mzn-inx-mes      pic  9(02)              .
      *        *-------------------------------------------------------*
      *        * Work per determinazione saldi da [mmz] per tipo conto *
      *        * merce 'T'                                             *
      *        *-------------------------------------------------------*
           05  w-det-mzt.
      *            *---------------------------------------------------*
      *            * Input                                             *
      *            *---------------------------------------------------*
               10  w-det-mzt-inp.
      *                *-----------------------------------------------*
      *                * Data attuale                                  *
      *                *-----------------------------------------------*
                   15  w-det-mzt-dat-att  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Data di riferimento per determinazione saldi  *
      *                *-----------------------------------------------*
                   15  w-det-mzt-dat-rif  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Flag indicatore se la data di riferimento e'  *
      *                * un fine mese                                  *
      *                *   - Spaces : No                               *
      *                *   - F      : Si                               *
      *                *-----------------------------------------------*
                   15  w-det-mzt-fin-mes  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Indice su mese di esercizio relativo alla da- *
      *                * ta di riferimento per determinazione saldi    *
      *                *-----------------------------------------------*
                   15  w-det-mzt-ism-rif  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Flag indicatore se necessario esplorare uno   *
      *                * o due esercizi                                *
      *                *   - 1 : 1 esercizio                           *
      *                *   - 2 : 2 esercizi                            *
      *                *-----------------------------------------------*
                   15  w-det-mzt-1o2-ese  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Primo anno di esercizio da considerare        *
      *                *-----------------------------------------------*
                   15  w-det-mzt-1mo-ese  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Secondo anno di esercizio da considerare      *
      *                *-----------------------------------------------*
                   15  w-det-mzt-2do-ese  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Tipo codice di magazzino                      *
      *                *-----------------------------------------------*
                   15  w-det-mzt-tip-mag  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico di magazzino                  *
      *                *-----------------------------------------------*
                   15  w-det-mzt-num-mag  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Una o tutte le dipendenze                     *
      *                *-----------------------------------------------*
                   15  w-det-mzt-uot-dpz  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza                             *
      *                *-----------------------------------------------*
                   15  w-det-mzt-cod-dpz  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Una o tutte le varianti                       *
      *                *-----------------------------------------------*
                   15  w-det-mzt-uot-var  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice variante                               *
      *                *-----------------------------------------------*
                   15  w-det-mzt-var-mag  pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Uno o tutti i conti merce                     *
      *                *-----------------------------------------------*
                   15  w-det-mzt-uot-ctm  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice conto merce                            *
      *                *-----------------------------------------------*
                   15  w-det-mzt-cod-ctm  pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Tipo archivio                                 *
      *                *  - Spaces : Tutti                             *
      *                *  - C      : Clienti                           *
      *                *  - F      : Fornitori                         *
      *                *  - A      : Agenti                            *
      *                *-----------------------------------------------*
                   15  w-det-mzt-tip-arc  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Uno o tutti i codici archivio                 *
      *                *-----------------------------------------------*
                   15  w-det-mzt-uot-arc  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice archivio                               *
      *                *-----------------------------------------------*
                   15  w-det-mzt-cod-arc  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Una o tutte le dipendenze per il codice ar-   *
      *                * chivio                                        *
      *                *-----------------------------------------------*
                   15  w-det-mzt-uot-dpa  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza per il codice archivio      *
      *                *-----------------------------------------------*
                   15  w-det-mzt-dpz-arc  pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Output                                            *
      *            *---------------------------------------------------*
               10  w-det-mzt-out.
      *                *-----------------------------------------------*
      *                * 1. esercizio   : Debito inizio anno           *
      *                *-----------------------------------------------*
                   15  w-det-mzt-1oe-dbi      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 1. esercizio   : Totale movimenti a nostro    *
      *                *                  credito anno                 *
      *                *-----------------------------------------------*
                   15  w-det-mzt-1oe-tca      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 1. esercizio   : Totale movimenti a nostro    *
      *                *                  debito anno                  *
      *                *-----------------------------------------------*
                   15  w-det-mzt-1oe-tda      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 2. esercizio   : Debito inizio anno           *
      *                *-----------------------------------------------*
                   15  w-det-mzt-2oe-dbi      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 2. esercizio   : Totale movimenti a nostro    *
      *                *                  credito periodo              *
      *                *-----------------------------------------------*
                   15  w-det-mzt-2oe-tcp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * 2. esercizio   : Totale movimenti a nostro    *
      *                *                  debito periodo               *
      *                *-----------------------------------------------*
                   15  w-det-mzt-2oe-tdp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Debito inizio anno           *
      *                *-----------------------------------------------*
                   15  w-det-mzt-det-dbi      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Totale movimenti a nostro    *
      *                *                  credito periodo              *
      *                *-----------------------------------------------*
                   15  w-det-mzt-det-tcp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Totale movimenti a nostro    *
      *                *                  debito periodo               *
      *                *-----------------------------------------------*
                   15  w-det-mzt-det-tdp      pic s9(08)v9(03)        .
      *                *-----------------------------------------------*
      *                * Determinazione : Debito fine periodo          *
      *                *-----------------------------------------------*
                   15  w-det-mzt-det-dbf      pic s9(08)v9(03)        .
      *            *---------------------------------------------------*
      *            * Sub-work                                          *
      *            *---------------------------------------------------*
               10  w-det-mzt-sub.
      *                *-----------------------------------------------*
      *                * Anno di esercizio per subroutines :           *
      *                *   - Start                                     *
      *                *   - Read next                                 *
      *                *   - Test max                                  *
      *                *   - Selezione                                 *
      *                *-----------------------------------------------*
                   15  w-det-mzt-sub-ese  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Data relativa al primo giorno del mese della  *
      *                * data di riferimento                           *
      *                *-----------------------------------------------*
                   15  w-det-mzt-g1m-ddr  pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Flags                                             *
      *            *---------------------------------------------------*
               10  w-det-mzt-flg.
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Start               *
      *                *-----------------------------------------------*
                   15  w-det-mzt-str-flg      pic  x(01)              .
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Read Next           *
      *                *-----------------------------------------------*
                   15  w-det-mzt-nxt-flg      pic  x(01)              .
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Test Max            *
      *                *-----------------------------------------------*
                   15  w-det-mzt-max-flg      pic  x(01)              .
      *                *-----------------------------------------------*
      *                * Flag di uscita operazioni Selezione           *
      *                *-----------------------------------------------*
                   15  w-det-mzt-sel-flg      pic  x(01)              .
      *            *---------------------------------------------------*
      *            * Indici                                            *
      *            *---------------------------------------------------*
               10  w-det-mzt-inx.
      *                *-----------------------------------------------*
      *                * Indice su mesi 1..12                          *
      *                *-----------------------------------------------*
                   15  w-det-mzt-inx-mes      pic  9(02)              .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-sld-mag              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-sld-mag-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        d-sld-mag-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-sld-mag-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-sld-mag-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Determinazione saldo di magazzino           *
      *                  *---------------------------------------------*
           else if   d-sld-mag-tip-ope    =    "SL"
                     perform dsd-000      thru dsd-999                .
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
      *              *-------------------------------------------------*
      *              * Lettura numerazione [datife]                    *
      *              *-------------------------------------------------*
           perform   dat-ife-000          thru dat-ife-999            .
      *              *-------------------------------------------------*
      *              * Determinazione mese chiusura esercizio per la   *
      *              * gestione magazzino                              *
      *              *-------------------------------------------------*
           perform   mes-che-000          thru mes-che-999            .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [mms]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                  *---------------------------------------------*
      *                  * [mmz]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                  *---------------------------------------------*
      *                  * [mmr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
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
      *                  * [mms]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                  *---------------------------------------------*
      *                  * [mmz]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                  *---------------------------------------------*
      *                  * [mmr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
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
                     move  spaces         to   d-sld-mag-exi-sts
           else      move  "#"            to   d-sld-mag-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldo di magazzino                         *
      *    *-----------------------------------------------------------*
       dsd-000.
      *              *-------------------------------------------------*
      *              * Determinazione valori utili per ogni tipo di    *
      *              * determinazione saldo                            *
      *              *-------------------------------------------------*
       dsd-020.
      *                  *---------------------------------------------*
      *                  * Data attuale                                *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-det-dsd-dat-att      .
       dsd-040.
      *                  *---------------------------------------------*
      *                  * Data di riferimento per determinazione sal- *
      *                  * di                                          *
      *                  *                                             *
      *                  * Da parametro passato, normalizzata al valo- *
      *                  * re della data attuale se passato un parame- *
      *                  * tro a zero                                  *
      *                  *---------------------------------------------*
           if        d-sld-mag-dat-sld    =    zero
                     move  w-det-dsd-dat-att
                                          to   w-det-dsd-dat-rif
           else      move  d-sld-mag-dat-sld
                                          to   w-det-dsd-dat-rif      .
       dsd-060.
      *                  *---------------------------------------------*
      *                  * Flag indicatore se la data di riferimento   *
      *                  * e' un fine mese oppure no                   *
      *                  *                                             *
      *                  * Si prova ad aggiungere 1 giorno alla data   *
      *                  * di riferimento, quindi si esegue il con-    *
      *                  * trollo sulla data, e se da' errore signi-   *
      *                  * fica che si e'  a fine mese                 *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-rif    to   s-dat                  .
           add       01                   to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     move  spaces         to   w-det-dsd-fin-mes
           else      move  "F"            to   w-det-dsd-fin-mes      .
       dsd-080.
      *                  *---------------------------------------------*
      *                  * Indice su mese di esercizio relativo alla   *
      *                  * data di riferimento per la determinazione   *
      *                  * saldi                                       *
      *                  *                                             *
      *                  * Se chiusura al mese 12 viene posto pari al  *
      *                  * mese della data di riferimento, altrimenti  *
      *                  * viene assestato                             *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-rif    to   s-dat                  .
           move      s-mes                to   w-det-dsd-ism-rif      .
           if        w-det-prg-mes-che    =    12
                     go to dsd-090.
           if        w-det-dsd-ism-rif    >    w-det-prg-mes-che
                     subtract  w-det-prg-mes-che
                                          from w-det-dsd-ism-rif
                     go to dsd-090.
           if        w-det-dsd-ism-rif    <    w-det-prg-mes-che
                     add      12          to   w-det-dsd-ism-rif
                     subtract  w-det-prg-mes-che
                                          from w-det-dsd-ism-rif
                     go to dsd-090.
           move      12                   to   w-det-dsd-ism-rif      .
       dsd-090.
      *                  *---------------------------------------------*
      *                  * Se si verificano le seguenti condizioni:    *
      *                  *                                             *
      *                  * - L'indice del mese di esercizio e' pari al *
      *                  *   dodicesimo mese                           *
      *                  * - La data di riferimento e' pari ad un fine *
      *                  *   mese                                      *
      *                  * - L'inventario di fine anno relativo all'e- *
      *                  *   sercizio relativo alla data di riferimen- *
      *                  *   to e' completamente chiuso                *
      *                  *                                             *
      *                  * si pone a zero l'indice sul mese di eserci- *
      *                  * zio, ad indicare che si richiede il saldo   *
      *                  * di inizio anno dell'esercizio successivo.   *
      *                  *---------------------------------------------*
           if        w-det-dsd-ism-rif    not  = 12
                     go to dsd-100.
           if        w-det-dsd-fin-mes    not  = "F"
                     go to dsd-100.
           move      w-det-dsd-dat-rif    to   s-dat                  .
           if        w-dnu-ife-ife-ese    <    s-saa
                     go to dsd-100.
           if        w-dnu-ife-ife-ese    >    s-saa
                     move  zero           to   w-det-dsd-ism-rif
                     go to dsd-100.
           if        w-dnu-ife-ife-flg    =    spaces
                     move  zero           to   w-det-dsd-ism-rif
                     go to dsd-100.
       dsd-100.
      *                  *---------------------------------------------*
      *                  * 2. anno di esercizio da considerare         *
      *                  *                                             *
      *                  * Anno di esercizio relativo alla data di ri- *
      *                  * ferimento per la determinazione dei saldi.  *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-rif    to   s-dat                  .
           if        w-det-prg-mes-che    =    12             or
                     s-mes                >    w-det-prg-mes-che
                     move     s-saa       to   w-det-dsd-2do-ese
           else      move     s-saa       to   w-det-dsd-2do-ese
                     subtract 1           from w-det-dsd-2do-ese      .
      *                  *---------------------------------------------*
      *                  * Se si sta' trattando l'inizio anno dell'e-  *
      *                  * sercizio successivo, si pone l'esercizio    *
      *                  * successivo.                                 *
      *                  *---------------------------------------------*
           if        w-det-dsd-ism-rif    =    zero
                     add   1              to   w-det-dsd-2do-ese      .
       dsd-120.
      *                  *---------------------------------------------*
      *                  * 1. anno di esercizio da considerare         *
      *                  *                                             *
      *                  * Anno di esercizio precedente al 2. anno di  *
      *                  * esercizio da considerare                    *
      *                  *---------------------------------------------*
           move      w-det-dsd-2do-ese    to   w-det-dsd-1mo-ese      .
           subtract  1                    from w-det-dsd-1mo-ese      .
      *                  *---------------------------------------------*
      *                  * Se si sta' trattando l'inizio anno dell'e-  *
      *                  * sercizio successivo, si pone l'esercizio    *
      *                  * a zero                                      *
      *                  *---------------------------------------------*
           if        w-det-dsd-ism-rif    =    zero
                     move  zero           to   w-det-dsd-1mo-ese      .
       dsd-140.
      *                  *---------------------------------------------*
      *                  * Flag indicatore se necessario esplorare uno *
      *                  * o due esercizi                              *
      *                  *                                             *
      *                  * - Se si sta' trattando l'inizio anno del-   *
      *                  *   l'esercizio successivo si considera co-   *
      *                  *   munque un solo esercizio                  *
      *                  *                                             *
      *                  * - Se non e' mai stata eseguita una chiusura *
      *                  *   si considerano comunque due esercizi      *
      *                  *                                             *
      *                  *   altrimenti                                *
      *                  *                                             *
      *                  * - Se l'esercizio relativo alla data di ri-  *
      *                  *   ferimento e' uguale o inferiore all'ul-   *
      *                  *   timo esercizio di chiusura si considera   *
      *                  *   un solo esercizio                         *
      *                  *                                             *
      *                  *   altrimenti                                *
      *                  *                                             *
      *                  * - Se si e' in corso di chiusura si conside- *
      *                  *   rano due esercizi                         *
      *                  *                                             *
      *                  *   altrimenti                                *
      *                  *                                             *
      *                  * - Se l'esercizio precedente a quello rela-  *
      *                  *   tivo alla data di riferimento e' pari al- *
      *                  *   l'esercizio dell'ultima chiusura si con-  *
      *                  *   sidera un solo esercizio                  *
      *                  *                                             *
      *                  *   altrimenti                                *
      *                  *                                             *
      *                  * - Si considerano due esercizi               *
      *                  *                                             *
      *                  *---------------------------------------------*
           if        w-det-dsd-ism-rif    =    zero
                     move  1              to   w-det-dsd-1o2-ese
           else if   w-dnu-ife-ife-ese    =    zero
                     move  2              to   w-det-dsd-1o2-ese
           else if   w-det-dsd-2do-ese    not  > w-dnu-ife-ife-ese
                     move  1              to   w-det-dsd-1o2-ese
           else if   w-dnu-ife-ife-flg    not  = spaces
                     move  2              to   w-det-dsd-1o2-ese
           else if   w-det-dsd-1mo-ese    =    w-dnu-ife-ife-ese
                     move  1              to   w-det-dsd-1o2-ese
           else      move  2              to   w-det-dsd-1o2-ese      .
       dsd-200.
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine di determinazione del *
      *              * saldo a seconda del tipo di determinazione ri-  *
      *              * chiesto                                         *
      *              *-------------------------------------------------*
           if        d-sld-mag-tip-sld    = 0000
                     perform   dsd-gcf-000
                                          thru dsd-gcf-999
           else if   d-sld-mag-tip-sld    = 0100
                     perform   dsd-gcp-000
                                          thru dsd-gcp-999
           else if   d-sld-mag-tip-sld    = 0200
                     perform   dsd-gcn-000
                                          thru dsd-gcn-999
           else if   d-sld-mag-tip-sld    = 0300
                     perform   dsd-gct-000
                                          thru dsd-gct-999
           else      perform   dsd-gcx-000
                                          thru dsd-gcx-999            .
       dsd-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dsd-999.
       dsd-999.
           exit.

      *    *===========================================================*
      *    * Lettura numerazione [datife]                              *
      *    *-----------------------------------------------------------*
       dat-ife-000.
      *              *-------------------------------------------------*
      *              * Apertura numerazione [datife]                   *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *              *-------------------------------------------------*
      *              * Normalizzazione record 'rn-dat-ife'             *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *              *-------------------------------------------------*
      *              * Lettura record 'rn-dat-ife'                     *
      *              *-------------------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *              *-------------------------------------------------*
      *              * Spostamento in area di comodo                   *
      *              *-------------------------------------------------*
           move      rn-dat-ife-flg-ife   to   w-dnu-ife-ife-flg      .
           move      rn-dat-ife-ini-ife   to   w-dnu-ife-ife-dti      .
           move      rn-dat-ife-fin-ife   to   w-dnu-ife-ife-dtf      .
           move      rn-dat-ife-ese-ife   to   w-dnu-ife-ife-ese      .
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
       dat-ife-999.
           exit.

      *    *===========================================================*
      *    * Determinazione mese di chiusura esercizio per la gestione *
      *    * magazzino, mutuandolo dal mese chiusura esercizio per la  *
      *    * contabilita' generale                                     *
      *    *-----------------------------------------------------------*
       mes-che-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[mes-chi]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione non esistente : si norma-  *
      *              * lizza il mese di chiusura esercizio a : 12      *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     move  12             to   w-det-prg-mes-che
           else      move  s-num          to   w-det-prg-mes-che      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione del valore letto      *
      *              *-------------------------------------------------*
           if        w-det-prg-mes-che    <    01 or
                     w-det-prg-mes-che    >    12
                     move  12             to   w-det-prg-mes-che      .
       mes-che-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldo tipo 0000                            *
      *    *                                                           *
      *    * Giacenza fisica                                           *
      *    *-----------------------------------------------------------*
       dsd-gcf-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo subroutine  *
      *              * di determinazione saldi da [mms]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data attuale                                *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-att    to   w-det-mms-dat-att      .
      *                  *---------------------------------------------*
      *                  * Data di riferimento per determinazione sal- *
      *                  * di                                          *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-rif    to   w-det-mms-dat-rif      .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di data di riferimento pari *
      *                  * ad un fine mese                             *
      *                  *---------------------------------------------*
           move      w-det-dsd-fin-mes    to   w-det-mms-fin-mes      .
      *                  *---------------------------------------------*
      *                  * Indice su mese di esercizio relativo alla   *
      *                  * data di riferimento per determinazione      *
      *                  * saldi                                       *
      *                  *---------------------------------------------*
           move      w-det-dsd-ism-rif    to   w-det-mms-ism-rif      .
      *                  *---------------------------------------------*
      *                  * Uno o due esercizi da considerare           *
      *                  *---------------------------------------------*
           move      w-det-dsd-1o2-ese    to   w-det-mms-1o2-ese      .
      *                  *---------------------------------------------*
      *                  * Primo anno di esercizio da considerare      *
      *                  *---------------------------------------------*
           move      w-det-dsd-1mo-ese    to   w-det-mms-1mo-ese      .
      *                  *---------------------------------------------*
      *                  * Secondo anno di esercizio da considerare    *
      *                  *---------------------------------------------*
           move      w-det-dsd-2do-ese    to   w-det-mms-2do-ese      .
      *                  *---------------------------------------------*
      *                  * Tipo codice di magazzino                    *
      *                  *---------------------------------------------*
           move      d-sld-mag-tip-mag    to   w-det-mms-tip-mag      .
      *                  *---------------------------------------------*
      *                  * Codice numerico di magazzino                *
      *                  *---------------------------------------------*
           move      d-sld-mag-num-mag    to   w-det-mms-num-mag      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze                   *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-dpz    to   w-det-mms-uot-dpz      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-dpz    to   w-det-mms-cod-dpz      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le varianti                     *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-var    to   w-det-mms-uot-var      .
      *                  *---------------------------------------------*
      *                  * Codice variante                             *
      *                  *---------------------------------------------*
           move      d-sld-mag-var-mag    to   w-det-mms-var-mag      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dislocazioni                 *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-dsl    to   w-det-mms-uot-dsl      .
      *                  *---------------------------------------------*
      *                  * Codice dislocazione                         *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-dsl    to   w-det-mms-cod-dsl      .
       dsd-gcf-100.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine di determinazione saldi da  *
      *              * [mms]                                           *
      *              *-------------------------------------------------*
           perform   det-mms-000          thru det-mms-999            .
       dsd-gcf-200.
      *              *-------------------------------------------------*
      *              * Risultati ottenuti dalla subroutine in valori   *
      *              * di uscita                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Giacenza fisica determinata                 *
      *                  *---------------------------------------------*
           move      w-det-mms-det-gff    to   d-sld-mag-sld-mag      .
      *                  *---------------------------------------------*
      *                  * Giacenza fisica ad inizio esercizio         *
      *                  *---------------------------------------------*
           move      w-det-mms-det-gfi    to   d-sld-mag-sub-s01      .
      *                  *---------------------------------------------*
      *                  * Totale carichi periodo                      *
      *                  *---------------------------------------------*
           move      w-det-mms-det-tcp    to   d-sld-mag-sub-s02      .
      *                  *---------------------------------------------*
      *                  * Totale scarichi periodo                     *
      *                  *---------------------------------------------*
           move      w-det-mms-det-tsp    to   d-sld-mag-sub-s03      .
       dsd-gcf-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dsd-gcf-999.
       dsd-gcf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldo tipo 0100                            *
      *    *                                                           *
      *    * Giacenza di proprieta'                                    *
      *    *-----------------------------------------------------------*
       dsd-gcp-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo subroutine  *
      *              * di determinazione saldi da [mms]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data attuale                                *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-att    to   w-det-mms-dat-att      .
      *                  *---------------------------------------------*
      *                  * Data di riferimento per determinazione sal- *
      *                  * di                                          *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-rif    to   w-det-mms-dat-rif      .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di data di riferimento pari *
      *                  * ad un fine mese                             *
      *                  *---------------------------------------------*
           move      w-det-dsd-fin-mes    to   w-det-mms-fin-mes      .
      *                  *---------------------------------------------*
      *                  * Indice su mese di esercizio relativo alla   *
      *                  * data di riferimento per determinazione      *
      *                  * saldi                                       *
      *                  *---------------------------------------------*
           move      w-det-dsd-ism-rif    to   w-det-mms-ism-rif      .
      *                  *---------------------------------------------*
      *                  * Uno o due esercizi da considerare           *
      *                  *---------------------------------------------*
           move      w-det-dsd-1o2-ese    to   w-det-mms-1o2-ese      .
      *                  *---------------------------------------------*
      *                  * Primo anno di esercizio da considerare      *
      *                  *---------------------------------------------*
           move      w-det-dsd-1mo-ese    to   w-det-mms-1mo-ese      .
      *                  *---------------------------------------------*
      *                  * Secondo anno di esercizio da considerare    *
      *                  *---------------------------------------------*
           move      w-det-dsd-2do-ese    to   w-det-mms-2do-ese      .
      *                  *---------------------------------------------*
      *                  * Tipo codice di magazzino                    *
      *                  *---------------------------------------------*
           move      d-sld-mag-tip-mag    to   w-det-mms-tip-mag      .
      *                  *---------------------------------------------*
      *                  * Codice numerico di magazzino                *
      *                  *---------------------------------------------*
           move      d-sld-mag-num-mag    to   w-det-mms-num-mag      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze                   *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-dpz    to   w-det-mms-uot-dpz      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-dpz    to   w-det-mms-cod-dpz      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le varianti                     *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-var    to   w-det-mms-uot-var      .
      *                  *---------------------------------------------*
      *                  * Codice variante                             *
      *                  *---------------------------------------------*
           move      d-sld-mag-var-mag    to   w-det-mms-var-mag      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dislocazioni                 *
      *                  *---------------------------------------------*
           move      "T"                  to   w-det-mms-uot-dsl      .
      *                  *---------------------------------------------*
      *                  * Codice dislocazione                         *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-mms-cod-dsl      .
       dsd-gcp-100.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine di determinazione saldi da  *
      *              * [mms]                                           *
      *              *-------------------------------------------------*
           perform   det-mms-000          thru det-mms-999            .
       dsd-gcp-200.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo subroutine  *
      *              * di determinazione saldi da [mmz] per tipo conto *
      *              * merce 'N'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data attuale                                *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-att    to   w-det-mzn-dat-att      .
      *                  *---------------------------------------------*
      *                  * Data di riferimento per determinazione sal- *
      *                  * di                                          *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-rif    to   w-det-mzn-dat-rif      .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di data di riferimento pari *
      *                  * ad un fine mese                             *
      *                  *---------------------------------------------*
           move      w-det-dsd-fin-mes    to   w-det-mzn-fin-mes      .
      *                  *---------------------------------------------*
      *                  * Indice su mese di esercizio relativo alla   *
      *                  * data di riferimento per determinazione      *
      *                  * saldi                                       *
      *                  *---------------------------------------------*
           move      w-det-dsd-ism-rif    to   w-det-mzn-ism-rif      .
      *                  *---------------------------------------------*
      *                  * Uno o due esercizi da considerare           *
      *                  *---------------------------------------------*
           move      w-det-dsd-1o2-ese    to   w-det-mzn-1o2-ese      .
      *                  *---------------------------------------------*
      *                  * Primo anno di esercizio da considerare      *
      *                  *---------------------------------------------*
           move      w-det-dsd-1mo-ese    to   w-det-mzn-1mo-ese      .
      *                  *---------------------------------------------*
      *                  * Secondo anno di esercizio da considerare    *
      *                  *---------------------------------------------*
           move      w-det-dsd-2do-ese    to   w-det-mzn-2do-ese      .
      *                  *---------------------------------------------*
      *                  * Tipo codice di magazzino                    *
      *                  *---------------------------------------------*
           move      d-sld-mag-tip-mag    to   w-det-mzn-tip-mag      .
      *                  *---------------------------------------------*
      *                  * Codice numerico di magazzino                *
      *                  *---------------------------------------------*
           move      d-sld-mag-num-mag    to   w-det-mzn-num-mag      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze                   *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-dpz    to   w-det-mzn-uot-dpz      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-dpz    to   w-det-mzn-cod-dpz      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le varianti                     *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-var    to   w-det-mzn-uot-var      .
      *                  *---------------------------------------------*
      *                  * Codice variante                             *
      *                  *---------------------------------------------*
           move      d-sld-mag-var-mag    to   w-det-mzn-var-mag      .
      *                  *---------------------------------------------*
      *                  * Uno o tutti i conti merce                   *
      *                  *---------------------------------------------*
           move      "T"                  to   w-det-mzn-uot-ctm      .
      *                  *---------------------------------------------*
      *                  * Codice conto merce                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-mzn-cod-ctm      .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-mzn-tip-arc      .
      *                  *---------------------------------------------*
      *                  * Uno o tutti i codici archivio               *
      *                  *---------------------------------------------*
           move      "T"                  to   w-det-mzn-uot-arc      .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzn-cod-arc      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze per il codice ar- *
      *                  * chivio                                      *
      *                  *---------------------------------------------*
           move      "T"                  to   w-det-mzn-uot-dpa      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza per il codice archivio    *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-mzn-dpz-arc      .
       dsd-gcp-300.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine di determinazione saldi da  *
      *              * [mmz] per tipo conto merce 'N'                  *
      *              *-------------------------------------------------*
           perform   det-mzn-000          thru det-mzn-999            .
       dsd-gcp-400.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo subroutine  *
      *              * di determinazione saldi da [mmz] per tipo conto *
      *              * merce 'T'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data attuale                                *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-att    to   w-det-mzt-dat-att      .
      *                  *---------------------------------------------*
      *                  * Data di riferimento per determinazione sal- *
      *                  * di                                          *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-rif    to   w-det-mzt-dat-rif      .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di data di riferimento pari *
      *                  * ad un fine mese                             *
      *                  *---------------------------------------------*
           move      w-det-dsd-fin-mes    to   w-det-mzt-fin-mes      .
      *                  *---------------------------------------------*
      *                  * Indice su mese di esercizio relativo alla   *
      *                  * data di riferimento per determinazione      *
      *                  * saldi                                       *
      *                  *---------------------------------------------*
           move      w-det-dsd-ism-rif    to   w-det-mzt-ism-rif      .
      *                  *---------------------------------------------*
      *                  * Uno o due esercizi da considerare           *
      *                  *---------------------------------------------*
           move      w-det-dsd-1o2-ese    to   w-det-mzt-1o2-ese      .
      *                  *---------------------------------------------*
      *                  * Primo anno di esercizio da considerare      *
      *                  *---------------------------------------------*
           move      w-det-dsd-1mo-ese    to   w-det-mzt-1mo-ese      .
      *                  *---------------------------------------------*
      *                  * Secondo anno di esercizio da considerare    *
      *                  *---------------------------------------------*
           move      w-det-dsd-2do-ese    to   w-det-mzt-2do-ese      .
      *                  *---------------------------------------------*
      *                  * Tipo codice di magazzino                    *
      *                  *---------------------------------------------*
           move      d-sld-mag-tip-mag    to   w-det-mzt-tip-mag      .
      *                  *---------------------------------------------*
      *                  * Codice numerico di magazzino                *
      *                  *---------------------------------------------*
           move      d-sld-mag-num-mag    to   w-det-mzt-num-mag      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze                   *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-dpz    to   w-det-mzt-uot-dpz      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-dpz    to   w-det-mzt-cod-dpz      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le varianti                     *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-var    to   w-det-mzt-uot-var      .
      *                  *---------------------------------------------*
      *                  * Codice variante                             *
      *                  *---------------------------------------------*
           move      d-sld-mag-var-mag    to   w-det-mzt-var-mag      .
      *                  *---------------------------------------------*
      *                  * Uno o tutti i conti merce                   *
      *                  *---------------------------------------------*
           move      "T"                  to   w-det-mzt-uot-ctm      .
      *                  *---------------------------------------------*
      *                  * Codice conto merce                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-mzt-cod-ctm      .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-mzt-tip-arc      .
      *                  *---------------------------------------------*
      *                  * Uno o tutti i codici archivio               *
      *                  *---------------------------------------------*
           move      "T"                  to   w-det-mzt-uot-arc      .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzt-cod-arc      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze per il codice ar- *
      *                  * chivio                                      *
      *                  *---------------------------------------------*
           move      "T"                  to   w-det-mzt-uot-dpa      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza per il codice archivio    *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-mzt-dpz-arc      .
       dsd-gcp-500.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine di determinazione saldi da  *
      *              * [mmz] per tipo conto merce 'T'                  *
      *              *-------------------------------------------------*
           perform   det-mzt-000          thru det-mzt-999            .
       dsd-gcp-600.
      *              *-------------------------------------------------*
      *              * Risultati ottenuti dalle subroutines in valori  *
      *              * di uscita                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Giacenza di proprieta' determinata          *
      *                  *                                             *
      *                  * + [mms] Giacenza fisica                     *
      *                  * + [mmz] Credito fine periodo                *
      *                  * - [mmz] Debito fine periodo                 *
      *                  *---------------------------------------------*
           move      w-det-mms-det-gff    to   d-sld-mag-sld-mag      .
           add       w-det-mzn-det-crf    to   d-sld-mag-sld-mag      .
           subtract  w-det-mzt-det-dbf    from d-sld-mag-sld-mag      .
      *                  *---------------------------------------------*
      *                  * Giacenza fisica determinata                 *
      *                  *                                             *
      *                  * + [mms] Giacenza fisica                     *
      *                  *---------------------------------------------*
           move      w-det-mms-det-gff    to   d-sld-mag-sub-s01      .
      *                  *---------------------------------------------*
      *                  * Merce presso terzi ma non di loro proprie-  *
      *                  * ta'                                         *
      *                  *                                             *
      *                  * + [mmz] Credito fine periodo                *
      *                  *---------------------------------------------*
           move      w-det-mzn-det-crf    to   d-sld-mag-sub-s02      .
      *                  *---------------------------------------------*
      *                  * Merce presso di noi ma non di nostra pro-   *
      *                  * prieta'                                     *
      *                  *                                             *
      *                  * + [mmz] Debito fine periodo                 *
      *                  *---------------------------------------------*
           move      w-det-mzt-det-dbf    to   d-sld-mag-sub-s03      .
       dsd-gcp-700.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dsd-gcp-999.
       dsd-gcp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldo tipo 0200                            *
      *    *                                                           *
      *    * Merce presso terzi ma non di loro proprieta'              *
      *    *-----------------------------------------------------------*
       dsd-gcn-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo subroutine  *
      *              * di determinazione saldi da [mmz] per tipo conto *
      *              * merce 'N'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data attuale                                *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-att    to   w-det-mzn-dat-att      .
      *                  *---------------------------------------------*
      *                  * Data di riferimento per determinazione sal- *
      *                  * di                                          *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-rif    to   w-det-mzn-dat-rif      .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di data di riferimento pari *
      *                  * ad un fine mese                             *
      *                  *---------------------------------------------*
           move      w-det-dsd-fin-mes    to   w-det-mzn-fin-mes      .
      *                  *---------------------------------------------*
      *                  * Indice su mese di esercizio relativo alla   *
      *                  * data di riferimento per determinazione      *
      *                  * saldi                                       *
      *                  *---------------------------------------------*
           move      w-det-dsd-ism-rif    to   w-det-mzn-ism-rif      .
      *                  *---------------------------------------------*
      *                  * Uno o due esercizi da considerare           *
      *                  *---------------------------------------------*
           move      w-det-dsd-1o2-ese    to   w-det-mzn-1o2-ese      .
      *                  *---------------------------------------------*
      *                  * Primo anno di esercizio da considerare      *
      *                  *---------------------------------------------*
           move      w-det-dsd-1mo-ese    to   w-det-mzn-1mo-ese      .
      *                  *---------------------------------------------*
      *                  * Secondo anno di esercizio da considerare    *
      *                  *---------------------------------------------*
           move      w-det-dsd-2do-ese    to   w-det-mzn-2do-ese      .
      *                  *---------------------------------------------*
      *                  * Tipo codice di magazzino                    *
      *                  *---------------------------------------------*
           move      d-sld-mag-tip-mag    to   w-det-mzn-tip-mag      .
      *                  *---------------------------------------------*
      *                  * Codice numerico di magazzino                *
      *                  *---------------------------------------------*
           move      d-sld-mag-num-mag    to   w-det-mzn-num-mag      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze                   *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-dpz    to   w-det-mzn-uot-dpz      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-dpz    to   w-det-mzn-cod-dpz      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le varianti                     *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-var    to   w-det-mzn-uot-var      .
      *                  *---------------------------------------------*
      *                  * Codice variante                             *
      *                  *---------------------------------------------*
           move      d-sld-mag-var-mag    to   w-det-mzn-var-mag      .
      *                  *---------------------------------------------*
      *                  * Uno o tutti i conti merce                   *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-ctm    to   w-det-mzn-uot-ctm      .
      *                  *---------------------------------------------*
      *                  * Codice conto merce                          *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-ctm    to   w-det-mzn-cod-ctm      .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           move      d-sld-mag-tip-arc    to   w-det-mzn-tip-arc      .
      *                  *---------------------------------------------*
      *                  * Uno o tutti i codici archivio               *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-arc    to   w-det-mzn-uot-arc      .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-arc    to   w-det-mzn-cod-arc      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze per il codice ar- *
      *                  * chivio                                      *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-dpa    to   w-det-mzn-uot-dpa      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza per il codice archivio    *
      *                  *---------------------------------------------*
           move      d-sld-mag-dpz-arc    to   w-det-mzn-dpz-arc      .
       dsd-gcn-100.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine di determinazione saldi da  *
      *              * [mmz] per tipo conto merce 'N'                  *
      *              *-------------------------------------------------*
           perform   det-mzn-000          thru det-mzn-999            .
       dsd-gcn-200.
      *              *-------------------------------------------------*
      *              * Risultati ottenuti dalla subroutine in valori   *
      *              * di uscita                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Credito fine periodo determinato            *
      *                  *---------------------------------------------*
           move      w-det-mzn-det-crf    to   d-sld-mag-sld-mag      .
      *                  *---------------------------------------------*
      *                  * Quantita' ad inizio anno                    *
      *                  *---------------------------------------------*
           move      w-det-mzn-det-cri    to   d-sld-mag-sub-s01      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti a nostro credito periodo   *
      *                  *---------------------------------------------*
           move      w-det-mzn-det-tcp    to   d-sld-mag-sub-s02      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti a nostro debito periodo    *
      *                  *---------------------------------------------*
           move      w-det-mzn-det-tdp    to   d-sld-mag-sub-s03      .
       dsd-gcn-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dsd-gcn-999.
       dsd-gcn-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldo tipo 0300                            *
      *    *                                                           *
      *    * Merce presso di noi ma non di nostra proprieta'           *
      *    *-----------------------------------------------------------*
       dsd-gct-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo subroutine  *
      *              * di determinazione saldi da [mmz] per tipo conto *
      *              * merce 'T'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data attuale                                *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-att    to   w-det-mzt-dat-att      .
      *                  *---------------------------------------------*
      *                  * Data di riferimento per determinazione sal- *
      *                  * di                                          *
      *                  *---------------------------------------------*
           move      w-det-dsd-dat-rif    to   w-det-mzt-dat-rif      .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di data di riferimento pari *
      *                  * ad un fine mese                             *
      *                  *---------------------------------------------*
           move      w-det-dsd-fin-mes    to   w-det-mzt-fin-mes      .
      *                  *---------------------------------------------*
      *                  * Indice su mese di esercizio relativo alla   *
      *                  * data di riferimento per determinazione      *
      *                  * saldi                                       *
      *                  *---------------------------------------------*
           move      w-det-dsd-ism-rif    to   w-det-mzt-ism-rif      .
      *                  *---------------------------------------------*
      *                  * Uno o due esercizi da considerare           *
      *                  *---------------------------------------------*
           move      w-det-dsd-1o2-ese    to   w-det-mzt-1o2-ese      .
      *                  *---------------------------------------------*
      *                  * Primo anno di esercizio da considerare      *
      *                  *---------------------------------------------*
           move      w-det-dsd-1mo-ese    to   w-det-mzt-1mo-ese      .
      *                  *---------------------------------------------*
      *                  * Secondo anno di esercizio da considerare    *
      *                  *---------------------------------------------*
           move      w-det-dsd-2do-ese    to   w-det-mzt-2do-ese      .
      *                  *---------------------------------------------*
      *                  * Tipo codice di magazzino                    *
      *                  *---------------------------------------------*
           move      d-sld-mag-tip-mag    to   w-det-mzt-tip-mag      .
      *                  *---------------------------------------------*
      *                  * Codice numerico di magazzino                *
      *                  *---------------------------------------------*
           move      d-sld-mag-num-mag    to   w-det-mzt-num-mag      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze                   *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-dpz    to   w-det-mzt-uot-dpz      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-dpz    to   w-det-mzt-cod-dpz      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le varianti                     *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-var    to   w-det-mzt-uot-var      .
      *                  *---------------------------------------------*
      *                  * Codice variante                             *
      *                  *---------------------------------------------*
           move      d-sld-mag-var-mag    to   w-det-mzt-var-mag      .
      *                  *---------------------------------------------*
      *                  * Uno o tutti i conti merce                   *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-ctm    to   w-det-mzt-uot-ctm      .
      *                  *---------------------------------------------*
      *                  * Codice conto merce                          *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-ctm    to   w-det-mzt-cod-ctm      .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           move      d-sld-mag-tip-arc    to   w-det-mzt-tip-arc      .
      *                  *---------------------------------------------*
      *                  * Uno o tutti i codici archivio               *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-arc    to   w-det-mzt-uot-arc      .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      d-sld-mag-cod-arc    to   w-det-mzt-cod-arc      .
      *                  *---------------------------------------------*
      *                  * Una o tutte le dipendenze per il codice ar- *
      *                  * chivio                                      *
      *                  *---------------------------------------------*
           move      d-sld-mag-uot-dpa    to   w-det-mzt-uot-dpa      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza per il codice archivio    *
      *                  *---------------------------------------------*
           move      d-sld-mag-dpz-arc    to   w-det-mzt-dpz-arc      .
       dsd-gct-100.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine di determinazione saldi da  *
      *              * [mmz] per tipo conto merce 'T'                  *
      *              *-------------------------------------------------*
           perform   det-mzt-000          thru det-mzt-999            .
       dsd-gct-200.
      *              *-------------------------------------------------*
      *              * Risultati ottenuti dalla subroutine in valori   *
      *              * di uscita                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Debito fine periodo determinato             *
      *                  *---------------------------------------------*
           move      w-det-mzt-det-dbf    to   d-sld-mag-sld-mag      .
      *                  *---------------------------------------------*
      *                  * Quantita' ad inizio anno                    *
      *                  *---------------------------------------------*
           move      w-det-mzt-det-dbi    to   d-sld-mag-sub-s01      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti a nostro credito periodo   *
      *                  *---------------------------------------------*
           move      w-det-mzt-det-tcp    to   d-sld-mag-sub-s02      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti a nostro debito periodo    *
      *                  *---------------------------------------------*
           move      w-det-mzt-det-tdp    to   d-sld-mag-sub-s03      .
       dsd-gct-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dsd-gct-999.
       dsd-gct-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldo di tipo non riconosciuto             *
      *    *-----------------------------------------------------------*
       dsd-gcx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori in uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   d-sld-mag-sld-mag      .
           move      zero                 to   d-sld-mag-sub-s01      .
           move      zero                 to   d-sld-mag-sub-s02      .
           move      zero                 to   d-sld-mag-sub-s03      .
       dsd-gcx-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldi da [mms]                             *
      *    *                                                           *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *  - w-det-mms-dat-att = Data attuale                       *
      *    *                                                           *
      *    *  - w-det-mms-dat-rif = Data di riferimento per determina- *
      *    *                        zione saldi                        *
      *    *                                                           *
      *    *  - w-det-mms-fin-mes = Flag indicatore di data di riferi- *
      *    *                        mento pari ad un fine mese         *
      *    *                                                           *
      *    *  - w-det-mms-ism-rif = Indice su mese di esercizio rela-  *
      *    *                        tivo alla data di riferimento per  *
      *    *                        determinazione saldi               *
      *    *                                                           *
      *    *  - w-det-mms-1o2-ese = Uno o due esercizi da considerare  *
      *    *                                                           *
      *    *  - w-det-mms-1mo-ese = Primo anno di esercizio da consi-  *
      *    *                        derare                             *
      *    *                                                           *
      *    *  - w-det-mms-2do-ese = Secondo anno di esercizio da con-  *
      *    *                        siderare                           *
      *    *                                                           *
      *    *  - w-det-mms-tip-mag = Tipo codice di magazzino           *
      *    *                                                           *
      *    *  - w-det-mms-num-mag = Codice numerico di magazzino       *
      *    *                                                           *
      *    *  - w-det-mms-uot-dpz = Una o tutte le dipendenze          *
      *    *                                                           *
      *    *  - w-det-mms-cod-dpz = Codice dipendenza                  *
      *    *                                                           *
      *    *  - w-det-mms-uot-var = Una o tutte le varianti            *
      *    *                                                           *
      *    *  - w-det-mms-var-mag = Codice variante                    *
      *    *                                                           *
      *    *  - w-det-mms-uot-dsl = Una o tutte le dislocazioni        *
      *    *                                                           *
      *    *  - w-det-mms-cod-dsl = Codice dislocazione                *
      *    *                                                           *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *                                                           *
      *    *  - w-det-mms-det-gfi = Giacenza fisica inizio esercizio   *
      *    *                                                           *
      *    *  - w-det-mms-det-tcp = Totale carichi periodo             *
      *    *                                                           *
      *    *  - w-det-mms-det-tsp = Totale scarichi periodo            *
      *    *                                                           *
      *    *  - w-det-mms-det-gff = Giacenza fisica fine periodo       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-mms-000.
      *              *-------------------------------------------------*
      *              * Determinazione saldi [mms] 1. esercizio         *
      *              *-------------------------------------------------*
           perform   det-mms-1oe-000      thru det-mms-1oe-999        .
       det-mms-050.
      *              *-------------------------------------------------*
      *              * Determinazione saldi [mms] 2. esercizio         *
      *              *-------------------------------------------------*
           perform   det-mms-2oe-000      thru det-mms-2oe-999        .
       det-mms-100.
      *              *-------------------------------------------------*
      *              * Determinazione saldi richiesti                  *
      *              *-------------------------------------------------*
       det-mms-150.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se 1 o 2 esercizi con- *
      *                  * siderati                                    *
      *                  *---------------------------------------------*
           if        w-det-mms-1o2-ese    =    2
                     go to det-mms-400.
       det-mms-200.
      *                  *---------------------------------------------*
      *                  * Se considerato un solo esercizio, ovvero il *
      *                  * secondo                                     *
      *                  *---------------------------------------------*
       det-mms-250.
      *                      *-----------------------------------------*
      *                      * Giacenza fisica inizio esercizio        *
      *                      *-----------------------------------------*
           move      w-det-mms-2oe-gfi    to   w-det-mms-det-gfi      .
      *                      *-----------------------------------------*
      *                      * Totale carichi periodo                  *
      *                      *-----------------------------------------*
           move      w-det-mms-2oe-tcp    to   w-det-mms-det-tcp      .
      *                      *-----------------------------------------*
      *                      * Totale scarichi periodo                 *
      *                      *-----------------------------------------*
           move      w-det-mms-2oe-tsp    to   w-det-mms-det-tsp      .
      *                      *-----------------------------------------*
      *                      * Giacenza fisica fine periodo            *
      *                      *-----------------------------------------*
           move      w-det-mms-det-gfi    to   w-det-mms-det-gff      .
           add       w-det-mms-det-tcp    to   w-det-mms-det-gff      .
           subtract  w-det-mms-det-tsp    from w-det-mms-det-gff      .
       det-mms-300.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-mms-999.
       det-mms-400.
      *                  *---------------------------------------------*
      *                  * Se considerati due esercizi                 *
      *                  *---------------------------------------------*
       det-mms-450.
      *                      *-----------------------------------------*
      *                      * Giacenza fisica inizio esercizio        *
      *                      *-----------------------------------------*
           move      w-det-mms-1oe-gfi    to   w-det-mms-det-gfi      .
           add       w-det-mms-1oe-tca    to   w-det-mms-det-gfi      .
           subtract  w-det-mms-1oe-tsa    from w-det-mms-det-gfi      .
      *                      *-----------------------------------------*
      *                      * Totale carichi periodo                  *
      *                      *-----------------------------------------*
           move      w-det-mms-2oe-tcp    to   w-det-mms-det-tcp      .
      *                      *-----------------------------------------*
      *                      * Totale scarichi periodo                 *
      *                      *-----------------------------------------*
           move      w-det-mms-2oe-tsp    to   w-det-mms-det-tsp      .
      *                      *-----------------------------------------*
      *                      * Giacenza fisica fine periodo            *
      *                      *-----------------------------------------*
           move      w-det-mms-det-gfi    to   w-det-mms-det-gff      .
           add       w-det-mms-det-tcp    to   w-det-mms-det-gff      .
           subtract  w-det-mms-det-tsp    from w-det-mms-det-gff      .
       det-mms-500.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-mms-999.
       det-mms-999.
           exit.

      *    *===========================================================*
      *    * Determinazioni saldi [mms] relative al 1. esercizio       *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-det-mms-1oe-gfi = Giacenza fisica inizio 1. eserci-  *
      *    *                        zio                                *
      *    *                                                           *
      *    *  - w-det-mms-1oe-tca = Totale carichi anno 1. esercizio   *
      *    *                                                           *
      *    *  - w-det-mms-1oe-tsa = Totale scarichi anno 1. esercizio  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-mms-1oe-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Giacenza fisica inizio 1. esercizio         *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mms-1oe-gfi      .
      *                  *---------------------------------------------*
      *                  * Totale carichi anno 1. esercizio            *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mms-1oe-tca      .
      *                  *---------------------------------------------*
      *                  * Totale scarichi anno 1. esercizio           *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mms-1oe-tsa      .
       det-mms-1oe-100.
      *              *-------------------------------------------------*
      *              * Se non sono da considerare due esercizi : usci- *
      *              * ta                                              *
      *              *-------------------------------------------------*
           if        w-det-mms-1o2-ese    not  = 2
                     go to det-mms-1oe-999.
       det-mms-1oe-200.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per subroutines di scan- *
      *              * sione [mms] per 1. esercizio                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anno di esercizio da considerare : il 1. e- *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      w-det-mms-1mo-ese    to   w-det-mms-sub-ese      .
       det-mms-1oe-300.
      *              *-------------------------------------------------*
      *              * Start su [mms] per 1. esercizio                 *
      *              *-------------------------------------------------*
           perform   det-mms-str-000      thru det-mms-str-999        .
      *              *-------------------------------------------------*
      *              * Se start errata : uscita                        *
      *              *-------------------------------------------------*
           if        w-det-mms-str-flg    not  = spaces
                     go to det-mms-1oe-999.
       det-mms-1oe-400.
      *              *-------------------------------------------------*
      *              * Read next su [mms] per 1. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mms-nxt-000      thru det-mms-nxt-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita                           *
      *              *-------------------------------------------------*
           if        w-det-mms-nxt-flg    not  = spaces
                     go to det-mms-1oe-999.
       det-mms-1oe-500.
      *              *-------------------------------------------------*
      *              * Test max su [mms] per 1. esercizio              *
      *              *-------------------------------------------------*
           perform   det-mms-max-000      thru det-mms-max-999        .
      *              *-------------------------------------------------*
      *              * Se test max non superato : uscita               *
      *              *-------------------------------------------------*
           if        w-det-mms-max-flg    not  = spaces
                     go to det-mms-1oe-999.
       det-mms-1oe-600.
      *              *-------------------------------------------------*
      *              * Selezioni su [mms] per 1. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mms-sel-000      thru det-mms-sel-999        .
      *              *-------------------------------------------------*
      *              * Se selezioni non superate : riciclo su Next     *
      *              *-------------------------------------------------*
           if        w-det-mms-sel-flg    not  = spaces
                     go to det-mms-1oe-400.
       det-mms-1oe-700.
      *              *-------------------------------------------------*
      *              * Elaborazioni sul record di [mms] in esame       *
      *              *-------------------------------------------------*
       det-mms-1oe-710.
      *                  *---------------------------------------------*
      *                  * Sommatoria della quantita' ad inizio eser-  *
      *                  * cizio                                       *
      *                  *---------------------------------------------*
           add       rf-mms-qta-ini       to   w-det-mms-1oe-gfi      .
       det-mms-1oe-720.
      *                  *---------------------------------------------*
      *                  * Sommatoria carichi e scarichi esercizio     *
      *                  *---------------------------------------------*
       det-mms-1oe-730.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice 1..12 su mesi   *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-mms-inx-mes      .
       det-mms-1oe-740.
      *                      *-----------------------------------------*
      *                      * Incremento indice su mesi               *
      *                      *-----------------------------------------*
           add       1                    to   w-det-mms-inx-mes      .
      *                      *-----------------------------------------*
      *                      * Se oltre ultimo mese : fine ciclo       *
      *                      *-----------------------------------------*
           if        w-det-mms-inx-mes    >    12
                     go to det-mms-1oe-750.
      *                      *-----------------------------------------*
      *                      * Sommatoria carichi esercizio            *
      *                      *-----------------------------------------*
           add       rf-mms-prg-car
                    (w-det-mms-inx-mes)   to   w-det-mms-1oe-tca      .
      *                      *-----------------------------------------*
      *                      * Sommatoria scarichi esercizio           *
      *                      *-----------------------------------------*
           add       rf-mms-prg-sca
                    (w-det-mms-inx-mes)   to   w-det-mms-1oe-tsa      .
      *                      *-----------------------------------------*
      *                      * Riciclo su mese successivo              *
      *                      *-----------------------------------------*
           go to     det-mms-1oe-740.
       det-mms-1oe-750.
      *              *-------------------------------------------------*
      *              * Riciclo su Next                                 *
      *              *-------------------------------------------------*
           go to     det-mms-1oe-400.
       det-mms-1oe-999.
           exit.

      *    *===========================================================*
      *    * Determinazioni saldi [mms] relative al 2. esercizio       *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-det-mms-2oe-gfi = Giacenza fisica inizio 2. eserci-  *
      *    *                        zio                                *
      *    *                                                           *
      *    *  - w-det-mms-2oe-tcp = Totale carichi periodo 2. eserci-  *
      *    *                        zio                                *
      *    *                                                           *
      *    *  - w-det-mms-2oe-tsp = Totale scarichi periodo 2. eserci- *
      *    *                        zio                                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-mms-2oe-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Giacenza fisica inizio 2. esercizio         *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mms-2oe-gfi      .
      *                  *---------------------------------------------*
      *                  * Totale carichi periodo 2. esercizio         *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mms-2oe-tcp      .
      *                  *---------------------------------------------*
      *                  * Totale scarichi periodo 2. esercizio        *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mms-2oe-tsp      .
       det-mms-2oe-050.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per subroutines di scan- *
      *              * sione [mms] per 2. esercizio                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anno di esercizio da considerare : il 2. e- *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      w-det-mms-2do-ese    to   w-det-mms-sub-ese      .
       det-mms-2oe-100.
      *              *-------------------------------------------------*
      *              * Start su [mms] per 2. esercizio                 *
      *              *-------------------------------------------------*
           perform   det-mms-str-000      thru det-mms-str-999        .
      *              *-------------------------------------------------*
      *              * Se start errata : a correzioni finali           *
      *              *-------------------------------------------------*
           if        w-det-mms-str-flg    not  = spaces
                     go to det-mms-2oe-450.
       det-mms-2oe-150.
      *              *-------------------------------------------------*
      *              * Read next su [mms] per 2. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mms-nxt-000      thru det-mms-nxt-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : a correzioni finali              *
      *              *-------------------------------------------------*
           if        w-det-mms-nxt-flg    not  = spaces
                     go to det-mms-2oe-450.
       det-mms-2oe-200.
      *              *-------------------------------------------------*
      *              * Test max su [mms] per 2. esercizio              *
      *              *-------------------------------------------------*
           perform   det-mms-max-000      thru det-mms-max-999        .
      *              *-------------------------------------------------*
      *              * Se test max non superato : a correzioni finali  *
      *              *-------------------------------------------------*
           if        w-det-mms-max-flg    not  = spaces
                     go to det-mms-2oe-450.
       det-mms-2oe-250.
      *              *-------------------------------------------------*
      *              * Selezioni su [mms] per 2. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mms-sel-000      thru det-mms-sel-999        .
      *              *-------------------------------------------------*
      *              * Se selezioni non superate : riciclo su Next     *
      *              *-------------------------------------------------*
           if        w-det-mms-sel-flg    not  = spaces
                     go to det-mms-2oe-150.
       det-mms-2oe-300.
      *              *-------------------------------------------------*
      *              * Elaborazioni sul record di [mms] in esame       *
      *              *-------------------------------------------------*
       det-mms-2oe-310.
      *                  *---------------------------------------------*
      *                  * Sommatoria della quantita' ad inizio eser-  *
      *                  * cizio                                       *
      *                  *---------------------------------------------*
           add       rf-mms-qta-ini       to   w-det-mms-2oe-gfi      .
       det-mms-2oe-320.
      *                  *---------------------------------------------*
      *                  * Sommatoria carichi e scarichi periodo ante- *
      *                  * cedente il mese di esercizio relativo alla  *
      *                  * data di riferimento per determinazione sal- *
      *                  * di                                          *
      *                  *---------------------------------------------*
       det-mms-2oe-325.
      *                      *-----------------------------------------*
      *                      * Se l'indice sul mese e' a zero signifi- *
      *                      * ca che interessa solamente il saldo di  *
      *                      * inizio esercizio, quindi si escludono   *
      *                      * tutte le altre sommatorie               *
      *                      *-----------------------------------------*
           if        w-det-mms-ism-rif    =    zero
                     go to det-mms-2oe-400.
       det-mms-2oe-330.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice su mesi pari    *
      *                      * all'indice su mese di esercizio rela-   *
      *                      * tivo alla data di riferimento per la    *
      *                      * determinazione saldi                    *
      *                      *-----------------------------------------*
           move      w-det-mms-ism-rif    to   w-det-mms-inx-mes      .
       det-mms-2oe-340.
      *                      *-----------------------------------------*
      *                      * Decremento indice su mesi               *
      *                      *-----------------------------------------*
           subtract  1                    from w-det-mms-inx-mes      .
      *                      *-----------------------------------------*
      *                      * Se giunti a zero : fine ciclo           *
      *                      *-----------------------------------------*
           if        w-det-mms-inx-mes    =    zero
                     go to det-mms-2oe-350.
      *                      *-----------------------------------------*
      *                      * Sommatoria carichi esercizio            *
      *                      *-----------------------------------------*
           add       rf-mms-prg-car
                    (w-det-mms-inx-mes)   to   w-det-mms-2oe-tcp      .
      *                      *-----------------------------------------*
      *                      * Sommatoria scarichi esercizio           *
      *                      *-----------------------------------------*
           add       rf-mms-prg-sca
                    (w-det-mms-inx-mes)   to   w-det-mms-2oe-tsp      .
      *                      *-----------------------------------------*
      *                      * Riciclo su mese precedente              *
      *                      *-----------------------------------------*
           go to     det-mms-2oe-340.
       det-mms-2oe-350.
      *                  *---------------------------------------------*
      *                  * Se la data di riferimento corrisponde alla  *
      *                  * data attuale oppure se la data di riferi-   *
      *                  * mento corrisponde ad un fine mese, si ac-   *
      *                  * cumulano anche i carichi e gli scarichi re- *
      *                  * lativi al mese di esercizio relativo alla   *
      *                  * data di riferimento per determinazione sal- *
      *                  * di                                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-det-mms-dat-rif    not  = w-det-mms-dat-att and
                     w-det-mms-fin-mes    not  = "F"
                     go to det-mms-2oe-400.
      *                      *-----------------------------------------*
      *                      * Sommatoria carichi esercizio            *
      *                      *-----------------------------------------*
           add       rf-mms-prg-car
                    (w-det-mms-ism-rif)   to   w-det-mms-2oe-tcp      .
      *                      *-----------------------------------------*
      *                      * Sommatoria scarichi esercizio           *
      *                      *-----------------------------------------*
           add       rf-mms-prg-sca
                    (w-det-mms-ism-rif)   to   w-det-mms-2oe-tsp      .
       det-mms-2oe-400.
      *              *-------------------------------------------------*
      *              * Riciclo su Next                                 *
      *              *-------------------------------------------------*
           go to     det-mms-2oe-150.
       det-mms-2oe-450.
      *              *-------------------------------------------------*
      *              * Correzioni finali                               *
      *              *-------------------------------------------------*
       det-mms-2oe-500.
      *                  *---------------------------------------------*
      *                  * Se l'indice sul mese e' a zero significa    *
      *                  * che interessa solamente il saldo di inizio  *
      *                  * esercizio, quindi si esce immediatamente    *
      *                  * senza effettuare le correzioni finali, in   *
      *                  * quanto non necessarie                       *
      *                  *---------------------------------------------*
           if        w-det-mms-ism-rif    =    zero
                     go to det-mms-2oe-999.
      *                  *---------------------------------------------*
      *                  * Se la data di riferimento corrisponde alla  *
      *                  * data attuale oppure se la data di riferi-   *
      *                  * mento corrisponde ad un fine mese, si esce  *
      *                  * immediatamente senza effettuare le corre-   *
      *                  * zioni finali, in quanto non necessarie      *
      *                  *---------------------------------------------*
           if        w-det-mms-dat-rif    =    w-det-mms-dat-att or
                     w-det-mms-fin-mes    =    "F"
                     go to det-mms-2oe-999.
      *                  *---------------------------------------------*
      *                  * Preparazione della data relativa al primo   *
      *                  * giorno del mese della della data di rife-   *
      *                  * rimento                                     *
      *                  *---------------------------------------------*
           move      w-det-mms-dat-rif    to   s-dat                  .
           move      01                   to   s-gio                  .
           move      s-dat                to   w-det-mms-g1m-ddr      .
       det-mms-2oe-550.
      *                  *---------------------------------------------*
      *                  * Start su [mmr]                              *
      *                  *---------------------------------------------*
       det-mms-2oe-555.
      *                      *-----------------------------------------*
      *                      * Deviazione se una o tutte le dipendenze *
      *                      *-----------------------------------------*
           if        w-det-mms-uot-dpz    =    "T"
                     go to det-mms-2oe-565.
       det-mms-2oe-560.
      *                      *-----------------------------------------*
      *                      * Se una sola dipendenza                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPZMAG    "         to   f-key                  .
           move      w-det-mms-cod-dpz    to   rf-mmr-cod-dpz         .
           move      w-det-mms-tip-mag    to   rf-mmr-tip-mag         .
           move      w-det-mms-num-mag    to   rf-mmr-num-mag         .
           move      w-det-mms-g1m-ddr    to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                          *-------------------------------------*
      *                          * A controllo esito Start             *
      *                          *-------------------------------------*
           go to     det-mms-2oe-570.
       det-mms-2oe-565.
      *                      *-----------------------------------------*
      *                      * Se tutte le dipendenze                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      w-det-mms-tip-mag    to   rf-mmr-tip-mag         .
           move      w-det-mms-num-mag    to   rf-mmr-num-mag         .
           move      w-det-mms-g1m-ddr    to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                          *-------------------------------------*
      *                          * A controllo esito Start             *
      *                          *-------------------------------------*
           go to     det-mms-2oe-570.
       det-mms-2oe-570.
      *                      *-----------------------------------------*
      *                      * Se Start errata : uscita                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-mms-2oe-999.
       det-mms-2oe-600.
      *                  *---------------------------------------------*
      *                  * Read Next su [mmr]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-mms-2oe-999.
       det-mms-2oe-650.
      *                  *---------------------------------------------*
      *                  * Test Max su [mmr], se non superato : uscita *
      *                  *---------------------------------------------*
       det-mms-2oe-655.
      *                      *-----------------------------------------*
      *                      * Deviazione se una o tutte le dipendenze *
      *                      *-----------------------------------------*
           if        w-det-mms-uot-dpz    =    "T"
                     go to det-mms-2oe-665.
       det-mms-2oe-660.
      *                      *-----------------------------------------*
      *                      * Se una sola dipendenza                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Max su codice dipendenza            *
      *                          *-------------------------------------*
           if        rf-mmr-cod-dpz       not  = w-det-mms-cod-dpz
                     go to det-mms-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su tipo codice magazzino        *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mag       not  = w-det-mms-tip-mag
                     go to det-mms-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su codice numerico magazzino    *
      *                          *-------------------------------------*
           if        rf-mmr-num-mag       not  = w-det-mms-num-mag
                     go to det-mms-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su data registrazione           *
      *                          *-------------------------------------*
           if        rf-mmr-dat-reg       >    w-det-mms-dat-rif
                     go to det-mms-2oe-999.
      *                          *-------------------------------------*
      *                          * Test max superato                   *
      *                          *-------------------------------------*
           go to     det-mms-2oe-700.
       det-mms-2oe-665.
      *                      *-----------------------------------------*
      *                      * Se tutte le dipendenze                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Max su tipo codice magazzino        *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mag       not  = w-det-mms-tip-mag
                     go to det-mms-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su codice numerico magazzino    *
      *                          *-------------------------------------*
           if        rf-mmr-num-mag       not  = w-det-mms-num-mag
                     go to det-mms-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su data registrazione           *
      *                          *-------------------------------------*
           if        rf-mmr-dat-reg       >    w-det-mms-dat-rif
                     go to det-mms-2oe-999.
      *                          *-------------------------------------*
      *                          * Test max superato                   *
      *                          *-------------------------------------*
           go to     det-mms-2oe-700.
       det-mms-2oe-700.
      *                  *---------------------------------------------*
      *                  * Selezione su [mmr], se non superata : rici- *
      *                  * clo a lettura                               *
      *                  *---------------------------------------------*
       det-mms-2oe-705.
      *                      *-----------------------------------------*
      *                      * Sel su data registrazione               *
      *                      *                                         *
      *                      * Se non compresa tra il primo giorno del *
      *                      * mese relativo alla data di riferimento, *
      *                      * e la data di riferimento stessa : sele- *
      *                      * zione non superata                      *
      *                      *-----------------------------------------*
           if        rf-mmr-dat-reg       <    w-det-mms-g1m-ddr or
                     rf-mmr-dat-reg       >    w-det-mms-dat-rif
                     go to det-mms-2oe-600.
       det-mms-2oe-710.
      *                      *-----------------------------------------*
      *                      * Sel su codice dipendenza                *
      *                      *                                         *
      *                      * Se una sola dipendenza, ma la dipenden- *
      *                      * za del movimento letto non corrisponde  *
      *                      * a quella richiesta : selezione non su-  *
      *                      * perata                                  *
      *                      *-----------------------------------------*
           if        w-det-mms-uot-dpz    not  = "T"
                     if    rf-mmr-cod-dpz not  = w-det-mms-cod-dpz
                           go to  det-mms-2oe-600.
       det-mms-2oe-715.
      *                      *-----------------------------------------*
      *                      * Sel su tipo di trattamento per il valo- *
      *                      * re per la causale                       *
      *                      *                                         *
      *                      * Se movimento di solo valore : selezione *
      *                      * non superata                            *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-val       =    "C" or
                     rf-mmr-trt-val       =    "X" or
                     rf-mmr-trt-val       =    "R" or
                     rf-mmr-trt-val       =    "Y"
                     go to det-mms-2oe-600.
       det-mms-2oe-720.
      *                      *-----------------------------------------*
      *                      * Sel su tipo movimentazione di magazzino *
      *                      *                                         *
      *                      * Se movimento di carico o scarico : se-  *
      *                      * lezione superata                        *
      *                      *                                         *
      *                      * Se tipo movimento non riconosciuto :    *
      *                      * selezione non superata                  *
      *                      *                                         *
      *                      * Se movimentazione interna               *
      *                      *                                         *
      *                      *    Se tutte le dislocazioni : selezione *
      *                      *    superata                             *
      *                      *                                         *
      *                      *    Se una sola dislocazione             *
      *                      *                                         *
      *                      *       Se almeno una delle due tra la    *
      *                      *       dislocazione origine o destina-   *
      *                      *       zione corrisponde alla disloca-   *
      *                      *       zione richiesta : selezione su-   *
      *                      *       perata                            *
      *                      *                                         *
      *                      *       Altrimenti : selezione non supe-  *
      *                      *       rata                              *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mdm       =    01 or
                     rf-mmr-tip-mdm       =    02
                     go to det-mms-2oe-725.
           if        rf-mmr-tip-mdm       not  = 03
                     go to det-mms-2oe-600.
           if        w-det-mms-uot-dsl    =    "T"
                     go to det-mms-2oe-725.
           if        rf-mmr-cod-dsl       =    w-det-mms-cod-dsl or
                     rf-mmr-cod-dsd       =    w-det-mms-cod-dsl
                     go to det-mms-2oe-725
           else      go to det-mms-2oe-600.
       det-mms-2oe-725.
      *                      *-----------------------------------------*
      *                      * Sel su trattamento conto merce e codice *
      *                      * conto merce                             *
      *                      *                                         *
      *                      * Se il movimento non puo' riferirsi ad   *
      *                      * un altro documento, oppure il movimen-  *
      *                      * to stesso e' una movimentazione di mer- *
      *                      * ce in conto : selezione superata        *
      *                      *                                         *
      *                      * Se tipo trattamento merce in conto non  *
      *                      * riconosciuto : selezione non superata   *
      *                      *                                         *
      *                      * Se il tipo trattamento merce in conto   *
      *                      * indica che il movimento puo' riferir-   *
      *                      * si, anche se non necessariamente, ad    *
      *                      * un precedente movimento di merce in     *
      *                      * conto                                   *
      *                      *                                         *
      *                      *    Se non e' stato citato alcun conto   *
      *                      *    merce : selezione superata           *
      *                      *                                         *
      *                      *    Altrimenti : selezione non superata  *
      *                      *                                         *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-mic       =    02 or
                     rf-mmr-trt-mic       =    03
                     go to det-mms-2oe-730.
           if        rf-mmr-trt-mic       not  = 01
                     go to det-mms-2oe-600.
           if        rf-mmr-cod-ctm       =    spaces
                     go to det-mms-2oe-730
           else      go to det-mms-2oe-600.
       det-mms-2oe-730.
      *                      *-----------------------------------------*
      *                      * Selezione su codice dislocazione        *
      *                      *                                         *
      *                      * Se si tratta di una movimentazione in-  *
      *                      * terna : selezione superata, in quanto   *
      *                      * il test e' gia' stato esercitato pre-   *
      *                      * cedentemente                            *
      *                      *                                         *
      *                      * Se tutte le dislocazioni : selezione    *
      *                      * superata                                *
      *                      *                                         *
      *                      * Se una sola dislocazione                *
      *                      *                                         *
      *                      *    Se il codice dislocazione effettivo  *
      *                      *    del movimento corrisponde al codice  *
      *                      *    dislocazione richiesto : selezione   *
      *                      *    superata                             *
      *                      *                                         *
      *                      *    Altrimenti : selezione non superata  *
      *                      *                                         *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mdm       =    03
                     go to det-mms-2oe-735.
           if        w-det-mms-uot-dsl    =    "T"
                     go to det-mms-2oe-735.
           if        rf-mmr-cod-dsl       =    w-det-mms-cod-dsl
                     go to det-mms-2oe-735
           else      go to det-mms-2oe-600.
       det-mms-2oe-735.
      *                      *-----------------------------------------*
      *                      * Sel su tipo codice magazzino            *
      *                      *                                         *
      *                      * Se non corrisponde a quello richiesto : *
      *                      * selezione non superata                  *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mag       not  = w-det-mms-tip-mag
                     go to det-mms-2oe-600.
       det-mms-2oe-740.
      *                      *-----------------------------------------*
      *                      * Sel su codice numerico magazzino        *
      *                      *                                         *
      *                      * Se non corrisponde a quello richiesto : *
      *                      * selezione non superata                  *
      *                      *-----------------------------------------*
           if        rf-mmr-num-mag       not  = w-det-mms-num-mag
                     go to det-mms-2oe-600.
       det-mms-2oe-745.
      *                      *-----------------------------------------*
      *                      * Sel su variante al codice magazzino     *
      *                      *                                         *
      *                      * Se tutte le varianti : selezione supe-  *
      *                      * rata                                    *
      *                      *                                         *
      *                      * Se una sola variante                    *
      *                      *                                         *
      *                      *    Se il codice variante del movimento  *
      *                      *    corrisponde al codice variante ri-   *
      *                      *    chiesto : selezione superata         *
      *                      *                                         *
      *                      *    Altrimenti : selezione non superata  *
      *                      *                                         *
      *                      *-----------------------------------------*
           if        w-det-mms-uot-var    =    "T"
                     go to det-mms-2oe-750.
           if        rf-mmr-var-mag       =    w-det-mms-var-mag
                     go to det-mms-2oe-750
           else      go to det-mms-2oe-600.
       det-mms-2oe-750.
      *                      *-----------------------------------------*
      *                      * Selezione su [mmr] superata             *
      *                      *-----------------------------------------*
           go to     det-mms-2oe-800.
       det-mms-2oe-800.
      *                  *---------------------------------------------*
      *                  * Elaborazioni sul record di [mmr] in esame : *
      *                  * simulazione di aggiornamento                *
      *                  *---------------------------------------------*
       det-mms-2oe-825.
      *                      *-----------------------------------------*
      *                      * Se movimentazione interna e richiesta   *
      *                      * per tutte le dislocazioni : nessun ag-  *
      *                      * giornamento, ne' per la prima, ne' per  *
      *                      * la seconda dislocazione                 *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mdm       =    03 and
                     w-det-mms-uot-dsl    =    "T"
                     go to det-mms-2oe-900.
       det-mms-2oe-850.
      *                      *-----------------------------------------*
      *                      * Aggiornamento relativo alla prima di-   *
      *                      * slocazione                              *
      *                      *-----------------------------------------*
       det-mms-2oe-855.
      *                          *-------------------------------------*
      *                          * Se una sola dislocazione e codice   *
      *                          * dislocazione diverso da quello ri-  *
      *                          * chiesto : no aggiornamento per la   *
      *                          * prima dislocazione                  *
      *                          *-------------------------------------*
           if        w-det-mms-uot-dsl    not  = "T"           and
                     rf-mmr-cod-dsl       not  = w-det-mms-cod-dsl
                     go to det-mms-2oe-875.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo movi- *
      *                          * mento                               *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mdm       =    01
                     go to det-mms-2oe-860
           else if   rf-mmr-tip-mdm       =    02 or
                     rf-mmr-tip-mdm       =    03
                     go to det-mms-2oe-865.
       det-mms-2oe-860.
      *                          *-------------------------------------*
      *                          * Se movimento di carico              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Sommatoria carichi esercizio    *
      *                              *---------------------------------*
           add       rf-mmr-qta-mov       to   w-det-mms-2oe-tcp      .
      *                              *---------------------------------*
      *                              * A seconda dislocazione          *
      *                              *---------------------------------*
           go to     det-mms-2oe-875.
       det-mms-2oe-865.
      *                          *-------------------------------------*
      *                          * Se movimento di scarico o movimen-  *
      *                          * tazione interna                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Sommatoria scarichi esercizio   *
      *                              *---------------------------------*
           add       rf-mmr-qta-mov       to   w-det-mms-2oe-tsp      .
      *                              *---------------------------------*
      *                              * A seconda dislocazione          *
      *                              *---------------------------------*
           go to     det-mms-2oe-875.
       det-mms-2oe-875.
      *                      *-----------------------------------------*
      *                      * Aggiornamento relativo alla seconda di- *
      *                      * slocazione                              *
      *                      *-----------------------------------------*
       det-mms-2oe-880.
      *                          *-------------------------------------*
      *                          * Se non si tratta di una movimenta-  *
      *                          * zione interna : no aggiornamento    *
      *                          * per la seconda dislocazione         *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mdm       not  = 03
                     go to det-mms-2oe-900.
      *                          *-------------------------------------*
      *                          * Se una sola dislocazione e codice   *
      *                          * dislocazione di destinazione diver- *
      *                          * so da quello richiesto : no aggior- *
      *                          * namento per la seconda dislocazione *
      *                          *-------------------------------------*
           if        w-det-mms-uot-dsl    not  = "T"           and
                     rf-mmr-cod-dsd       not  = w-det-mms-cod-dsl
                     go to det-mms-2oe-900.
      *                          *-------------------------------------*
      *                          * Sommatoria carichi esercizio        *
      *                          *-------------------------------------*
           add       rf-mmr-qta-mov       to   w-det-mms-2oe-tcp      .
       det-mms-2oe-900.
      *                      *-----------------------------------------*
      *                      * Fine elaborazioni su record di [mmr] in *
      *                      * esame                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Riciclo su Read Next su [mmr]       *
      *                          *-------------------------------------*
           go to     det-mms-2oe-600.
       det-mms-2oe-999.
           exit.

      *    *===========================================================*
      *    * Start su [mms] per determinazione saldi da [mms]          *
      *    *-----------------------------------------------------------*
       det-mms-str-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mms-str-flg      .
       det-mms-str-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se una o tutte le dipen-   *
      *              * denze                                           *
      *              *-------------------------------------------------*
           if        w-det-mms-uot-dpz    =    "T"
                     go to det-mms-str-300.
       det-mms-str-200.
      *              *-------------------------------------------------*
      *              * Se una sola dipendenza                          *
      *              *-------------------------------------------------*
       det-mms-str-220.
      *                  *---------------------------------------------*
      *                  * Start per chiave 'DPMGDS'                   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPMGDS    "         to   f-key                  .
           move      w-det-mms-sub-ese    to   rf-mms-ann-ese         .
           move      w-det-mms-cod-dpz    to   rf-mms-cod-dpz         .
           move      w-det-mms-tip-mag    to   rf-mms-tip-mag         .
           move      w-det-mms-num-mag    to   rf-mms-num-mag         .
           if        w-det-mms-uot-var    =    "T"
                     move  spaces         to   rf-mms-var-mag
           else      move  w-det-mms-var-mag
                                          to   rf-mms-var-mag         .
           if        w-det-mms-uot-dsl    =    "T"
                     move  spaces         to   rf-mms-cod-dsl
           else      move  w-det-mms-cod-dsl
                                          to   rf-mms-cod-dsl         .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
       det-mms-str-240.
      *                  *---------------------------------------------*
      *                  * Se start errata : uscita con flag di errore *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mms-str-flg
                     go to det-mms-str-999.
       det-mms-str-260.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-mms-str-999.
       det-mms-str-300.
      *              *-------------------------------------------------*
      *              * Se tutte le dipendenze                          *
      *              *-------------------------------------------------*
       det-mms-str-320.
      *                  *---------------------------------------------*
      *                  * Start per chiave 'MGDPDS'                   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MGDPDS    "         to   f-key                  .
           move      w-det-mms-sub-ese    to   rf-mms-ann-ese         .
           move      w-det-mms-tip-mag    to   rf-mms-tip-mag         .
           move      w-det-mms-num-mag    to   rf-mms-num-mag         .
           if        w-det-mms-uot-var    =    "T"
                     move  spaces         to   rf-mms-var-mag
           else      move  w-det-mms-var-mag
                                          to   rf-mms-var-mag         .
           move      zero                 to   rf-mms-cod-dpz         .
           if        w-det-mms-uot-dsl    =    "T"
                     move  spaces         to   rf-mms-cod-dsl
           else      move  w-det-mms-cod-dsl
                                          to   rf-mms-cod-dsl         .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
       det-mms-str-340.
      *                  *---------------------------------------------*
      *                  * Se start errata : uscita con flag di errore *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mms-str-flg
                     go to det-mms-str-999.
       det-mms-str-360.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-mms-str-999.
       det-mms-str-999.
           exit.

      *    *===========================================================*
      *    * Read next su [mms] per determinazione saldi da [mms]      *
      *    *-----------------------------------------------------------*
       det-mms-nxt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mms-nxt-flg      .
       det-mms-nxt-100.
      *              *-------------------------------------------------*
      *              * Read Next                                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
       det-mms-nxt-200.
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag di errore        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mms-nxt-flg
                     go to det-mms-nxt-999.
       det-mms-nxt-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-mms-nxt-999.
       det-mms-nxt-999.
           exit.

      *    *===========================================================*
      *    * Test max su [mms] per determinazione saldi da [mms]       *
      *    *-----------------------------------------------------------*
       det-mms-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mms-max-flg      .
       det-mms-max-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se una o tutte le dipen-   *
      *              * denze                                           *
      *              *-------------------------------------------------*
           if        w-det-mms-uot-dpz    =    "T"
                     go to det-mms-max-300.
       det-mms-max-200.
      *              *-------------------------------------------------*
      *              * Se una sola dipendenza                          *
      *              *-------------------------------------------------*
       det-mms-max-210.
      *                  *---------------------------------------------*
      *                  * Test su anno di esercizio                   *
      *                  *---------------------------------------------*
           if        rf-mms-ann-ese       not  = w-det-mms-sub-ese
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-220.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-mms-cod-dpz       not  = w-det-mms-cod-dpz
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-230.
      *                  *---------------------------------------------*
      *                  * Test su tipo codice di magazzino            *
      *                  *---------------------------------------------*
           if        rf-mms-tip-mag       not  = w-det-mms-tip-mag
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-240.
      *                  *---------------------------------------------*
      *                  * Test su codice numerico di magazzino        *
      *                  *---------------------------------------------*
           if        rf-mms-num-mag       not  = w-det-mms-num-mag
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-250.
      *                  *---------------------------------------------*
      *                  * Test su codice variante                     *
      *                  *---------------------------------------------*
           if        w-det-mms-uot-var    =    "T"
                     go to det-mms-max-260.
           if        rf-mms-var-mag       not  = w-det-mms-var-mag
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-260.
      *                  *---------------------------------------------*
      *                  * Test su codice dislocazione                 *
      *                  *---------------------------------------------*
           if        w-det-mms-uot-var    =    "T"
                     go to det-mms-max-270.
           if        w-det-mms-uot-dsl    =    "T"
                     go to det-mms-max-270.
           if        rf-mms-cod-dsl       not  = w-det-mms-cod-dsl
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-270.
      *                  *---------------------------------------------*
      *                  * Uscita per test superato                    *
      *                  *---------------------------------------------*
           go to     det-mms-max-999.
       det-mms-max-300.
      *              *-------------------------------------------------*
      *              * Se tutte le dipendenze                          *
      *              *-------------------------------------------------*
       det-mms-max-310.
      *                  *---------------------------------------------*
      *                  * Test su anno di esercizio                   *
      *                  *---------------------------------------------*
           if        rf-mms-ann-ese       not  = w-det-mms-sub-ese
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-320.
      *                  *---------------------------------------------*
      *                  * Test su tipo codice di magazzino            *
      *                  *---------------------------------------------*
           if        rf-mms-tip-mag       not  = w-det-mms-tip-mag
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-330.
      *                  *---------------------------------------------*
      *                  * Test su codice numerico di magazzino        *
      *                  *---------------------------------------------*
           if        rf-mms-num-mag       not  = w-det-mms-num-mag
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-340.
      *                  *---------------------------------------------*
      *                  * Test su codice variante                     *
      *                  *---------------------------------------------*
           if        w-det-mms-uot-var    =    "T"
                     go to det-mms-max-350.
           if        rf-mms-var-mag       not  = w-det-mms-var-mag
                     move  "#"            to   w-det-mms-max-flg
                     go to det-mms-max-999.
       det-mms-max-350.
      *                  *---------------------------------------------*
      *                  * Uscita per test superato                    *
      *                  *---------------------------------------------*
           go to     det-mms-max-999.
       det-mms-max-999.
           exit.

      *    *===========================================================*
      *    * Selezione su [mms] per determinazione saldi da [mms]      *
      *    *-----------------------------------------------------------*
       det-mms-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mms-sel-flg      .
       det-mms-sel-100.
      *              *-------------------------------------------------*
      *              * Selezione su anno di esercizio                  *
      *              *-------------------------------------------------*
           if        rf-mms-ann-ese       not  = w-det-mms-sub-ese
                     move  "#"            to   w-det-mms-sel-flg
                     go to det-mms-sel-999.
       det-mms-sel-150.
      *              *-------------------------------------------------*
      *              * Selezione su codice dipendenza                  *
      *              *-------------------------------------------------*
           if        w-det-mms-uot-dpz    =    "T"
                     go to det-mms-sel-200.
           if        rf-mms-cod-dpz       not  = w-det-mms-cod-dpz
                     move  "#"            to   w-det-mms-sel-flg
                     go to det-mms-sel-999.
       det-mms-sel-200.
      *              *-------------------------------------------------*
      *              * Selezione su tipo codice di magazzino           *
      *              *-------------------------------------------------*
           if        rf-mms-tip-mag       not  = w-det-mms-tip-mag
                     move  "#"            to   w-det-mms-sel-flg
                     go to det-mms-sel-999.
       det-mms-sel-250.
      *              *-------------------------------------------------*
      *              * Selezione su codice numerico di magazzino       *
      *              *-------------------------------------------------*
           if        rf-mms-num-mag       not  = w-det-mms-num-mag
                     move  "#"            to   w-det-mms-sel-flg
                     go to det-mms-sel-999.
       det-mms-sel-300.
      *              *-------------------------------------------------*
      *              * Selezione su codice variante                    *
      *              *-------------------------------------------------*
           if        w-det-mms-uot-var    =    "T"
                     go to det-mms-sel-350.
           if        rf-mms-var-mag       not  = w-det-mms-var-mag
                     move  "#"            to   w-det-mms-sel-flg
                     go to det-mms-sel-999.
       det-mms-sel-350.
      *              *-------------------------------------------------*
      *              * Selezione su codice dislocazione                *
      *              *-------------------------------------------------*
           if        w-det-mms-uot-dsl    =    "T"
                     go to det-mms-sel-400.
           if        rf-mms-cod-dsl       not  = w-det-mms-cod-dsl
                     move  "#"            to   w-det-mms-sel-flg
                     go to det-mms-sel-999.
       det-mms-sel-400.
      *              *-------------------------------------------------*
      *              * Uscita per selezione superata                   *
      *              *-------------------------------------------------*
           go to     det-mms-sel-999.
       det-mms-sel-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldi da [mmz] per tipo conto merce 'N'    *
      *    *                                                           *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *  - w-det-mzn-dat-att = Data attuale                       *
      *    *                                                           *
      *    *  - w-det-mzn-dat-rif = Data di riferimento per determina- *
      *    *                        zione saldi                        *
      *    *                                                           *
      *    *  - w-det-mzn-fin-mes = Flag indicatore di data di riferi- *
      *    *                        mento pari ad un fine mese         *
      *    *                                                           *
      *    *  - w-det-mzn-ism-rif = Indice su mese di esercizio rela-  *
      *    *                        tivo alla data di riferimento per  *
      *    *                        determinazione saldi               *
      *    *                                                           *
      *    *  - w-det-mzn-1o2-ese = Uno o due esercizi da considerare  *
      *    *                                                           *
      *    *  - w-det-mzn-1mo-ese = Primo anno di esercizio da consi-  *
      *    *                        derare                             *
      *    *                                                           *
      *    *  - w-det-mzn-2do-ese = Secondo anno di esercizio da con-  *
      *    *                        siderare                           *
      *    *                                                           *
      *    *  - w-det-mzn-tip-mag = Tipo codice di magazzino           *
      *    *                                                           *
      *    *  - w-det-mzn-num-mag = Codice numerico di magazzino       *
      *    *                                                           *
      *    *  - w-det-mzn-uot-dpz = Una o tutte le dipendenze          *
      *    *                                                           *
      *    *  - w-det-mzn-cod-dpz = Codice dipendenza                  *
      *    *                                                           *
      *    *  - w-det-mzn-uot-var = Una o tutte le varianti            *
      *    *                                                           *
      *    *  - w-det-mzn-var-mag = Codice variante                    *
      *    *                                                           *
      *    *  - w-det-mzn-uot-ctm = Uno o tutti i conti merce          *
      *    *                                                           *
      *    *  - w-det-mzn-cod-ctm = Codice conto merce                 *
      *    *                                                           *
      *    *  - w-det-mzn-tip-arc = Tipo archivio                      *
      *    *                                                           *
      *    *  - w-det-mzn-uot-arc = Uno o tutti i codici archivio      *
      *    *                                                           *
      *    *  - w-det-mzn-cod-arc = Codice archivio                    *
      *    *                                                           *
      *    *  - w-det-mzn-uot-dpa = Una o tutte le dipendenze per il   *
      *    *                        codice archivio                    *
      *    *                                                           *
      *    *  - w-det-mzn-dpz-arc = Codice dipendenza per il codice    *
      *    *                        archivio                           *
      *    *                                                           *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *                                                           *
      *    *  - w-det-mzn-det-cri = Credito inizio anno                *
      *    *                                                           *
      *    *  - w-det-mzn-det-tcp = Totale movimenti a nostro credito  *
      *    *                        periodo                            *
      *    *                                                           *
      *    *  - w-det-mzn-det-tdp = Totale movimenti a nostro debito   *
      *    *                        periodo                            *
      *    *                                                           *
      *    *  - w-det-mzn-det-crf = Credito fine periodo               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-mzn-000.
      *              *-------------------------------------------------*
      *              * Determinazione saldi [mmz] tipo conto merce 'N' *
      *              * 1. esercizio                                    *
      *              *-------------------------------------------------*
           perform   det-mzn-1oe-000      thru det-mzn-1oe-999        .
       det-mzn-050.
      *              *-------------------------------------------------*
      *              * Determinazione saldi [mmz] tipo conto merce 'N' *
      *              * 2. esercizio                                    *
      *              *-------------------------------------------------*
           perform   det-mzn-2oe-000      thru det-mzn-2oe-999        .
       det-mzn-100.
      *              *-------------------------------------------------*
      *              * Determinazione saldi richiesti                  *
      *              *-------------------------------------------------*
       det-mzn-150.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se 1 o 2 esercizi con- *
      *                  * siderati                                    *
      *                  *---------------------------------------------*
           if        w-det-mzn-1o2-ese    =    2
                     go to det-mzn-400.
       det-mzn-200.
      *                  *---------------------------------------------*
      *                  * Se considerato un solo esercizio, ovvero il *
      *                  * secondo                                     *
      *                  *---------------------------------------------*
       det-mzn-250.
      *                      *-----------------------------------------*
      *                      * Credito inizio anno                     *
      *                      *-----------------------------------------*
           move      w-det-mzn-2oe-cri    to   w-det-mzn-det-cri      .
      *                      *-----------------------------------------*
      *                      * Totale movimenti a nostro credito del   *
      *                      * periodo                                 *
      *                      *-----------------------------------------*
           move      w-det-mzn-2oe-tcp    to   w-det-mzn-det-tcp      .
      *                      *-----------------------------------------*
      *                      * Totale movimenti a nostro debito del    *
      *                      * periodo                                 *
      *                      *-----------------------------------------*
           move      w-det-mzn-2oe-tdp    to   w-det-mzn-det-tdp      .
      *                      *-----------------------------------------*
      *                      * Credito fine periodo                    *
      *                      *-----------------------------------------*
           move      w-det-mzn-det-cri    to   w-det-mzn-det-crf      .
           add       w-det-mzn-det-tcp    to   w-det-mzn-det-crf      .
           subtract  w-det-mzn-det-tdp    from w-det-mzn-det-crf      .
       det-mzn-300.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-mzn-999.
       det-mzn-400.
      *                  *---------------------------------------------*
      *                  * Se considerati due esercizi                 *
      *                  *---------------------------------------------*
       det-mzn-450.
      *                      *-----------------------------------------*
      *                      * Credito inizio anno                     *
      *                      *-----------------------------------------*
           move      w-det-mzn-1oe-cri    to   w-det-mzn-det-cri      .
           add       w-det-mzn-1oe-tca    to   w-det-mzn-det-cri      .
           subtract  w-det-mzn-1oe-tda    from w-det-mzn-det-cri      .
      *                      *-----------------------------------------*
      *                      * Totale movimenti a nostro credito del   *
      *                      * periodo                                 *
      *                      *-----------------------------------------*
           move      w-det-mzn-2oe-tcp    to   w-det-mzn-det-tcp      .
      *                      *-----------------------------------------*
      *                      * Totale movimenti a nostro debito del    *
      *                      * periodo                                 *
      *                      *-----------------------------------------*
           move      w-det-mzn-2oe-tdp    to   w-det-mzn-det-tdp      .
      *                      *-----------------------------------------*
      *                      * Credito fine periodo                    *
      *                      *-----------------------------------------*
           move      w-det-mzn-det-cri    to   w-det-mzn-det-crf      .
           add       w-det-mzn-det-tcp    to   w-det-mzn-det-crf      .
           subtract  w-det-mzn-det-tdp    from w-det-mzn-det-crf      .
       det-mzn-500.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-mzn-999.
       det-mzn-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldi [mmz] tipo conto merce 'N' relative  *
      *    * al 1. esercizio                                           *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-det-mzn-1oe-cri = Credito inizio anno 1. esercizio   *
      *    *                                                           *
      *    *  - w-det-mzn-1oe-tca = Totale movimenti a nostro credito  *
      *    *                        anno 1. esercizio                  *
      *    *                                                           *
      *    *  - w-det-mzn-1oe-tda = Totale movimenti a nostro debito   *
      *    *                        anno 1. esercizio                  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-mzn-1oe-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Credito inizio anno 1. esercizio            *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzn-1oe-cri      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti nostro credito anno 1. e-  *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzn-1oe-tca      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti nostro debito anno 1. e-   *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzn-1oe-tda      .
       det-mzn-1oe-100.
      *              *-------------------------------------------------*
      *              * Se non sono da considerare due esercizi : usci- *
      *              * ta                                              *
      *              *-------------------------------------------------*
           if        w-det-mzn-1o2-ese    not  = 2
                     go to det-mzn-1oe-999.
       det-mzn-1oe-200.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per subroutines di scan- *
      *              * sione [mmz] per 1. esercizio                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anno di esercizio da considerare : il 1. e- *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      w-det-mzn-1mo-ese    to   w-det-mzn-sub-ese      .
       det-mzn-1oe-300.
      *              *-------------------------------------------------*
      *              * Start su [mmz] per 1. esercizio                 *
      *              *-------------------------------------------------*
           perform   det-mzn-str-000      thru det-mzn-str-999        .
      *              *-------------------------------------------------*
      *              * Se start errata : uscita                        *
      *              *-------------------------------------------------*
           if        w-det-mzn-str-flg    not  = spaces
                     go to det-mzn-1oe-999.
       det-mzn-1oe-400.
      *              *-------------------------------------------------*
      *              * Read next su [mmz] per 1. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mzn-nxt-000      thru det-mzn-nxt-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita                           *
      *              *-------------------------------------------------*
           if        w-det-mzn-nxt-flg    not  = spaces
                     go to det-mzn-1oe-999.
       det-mzn-1oe-500.
      *              *-------------------------------------------------*
      *              * Test max su [mmz] per 1. esercizio              *
      *              *-------------------------------------------------*
           perform   det-mzn-max-000      thru det-mzn-max-999        .
      *              *-------------------------------------------------*
      *              * Se test max non superato : uscita               *
      *              *-------------------------------------------------*
           if        w-det-mzn-max-flg    not  = spaces
                     go to det-mzn-1oe-999.
       det-mzn-1oe-600.
      *              *-------------------------------------------------*
      *              * Selezioni su [mmz] per 1. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mzn-sel-000      thru det-mzn-sel-999        .
      *              *-------------------------------------------------*
      *              * Se selezioni non superate : riciclo su Next     *
      *              *-------------------------------------------------*
           if        w-det-mzn-sel-flg    not  = spaces
                     go to det-mzn-1oe-400.
       det-mzn-1oe-700.
      *              *-------------------------------------------------*
      *              * Elaborazioni sul record di [mmz] in esame       *
      *              *-------------------------------------------------*
       det-mzn-1oe-710.
      *                  *---------------------------------------------*
      *                  * Sommatoria della quantita' a nostro credito *
      *                  * ad inizio esercizio                         *
      *                  *---------------------------------------------*
           add       rf-mmz-qta-ini       to   w-det-mzn-1oe-cri      .
       det-mzn-1oe-720.
      *                  *---------------------------------------------*
      *                  * Sommatoria movimenti a nostro credito e a   *
      *                  * nostro debito per l'intero esercizio        *
      *                  *---------------------------------------------*
       det-mzn-1oe-730.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice 1..12 su mesi   *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-mzn-inx-mes      .
       det-mzn-1oe-740.
      *                      *-----------------------------------------*
      *                      * Incremento indice su mesi               *
      *                      *-----------------------------------------*
           add       1                    to   w-det-mzn-inx-mes      .
      *                      *-----------------------------------------*
      *                      * Se oltre ultimo mese : fine ciclo       *
      *                      *-----------------------------------------*
           if        w-det-mzn-inx-mes    >    12
                     go to det-mzn-1oe-750.
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro credito   *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-cre
                    (w-det-mzn-inx-mes)   to   w-det-mzn-1oe-tca      .
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro debito    *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-deb
                    (w-det-mzn-inx-mes)   to   w-det-mzn-1oe-tda      .
      *                      *-----------------------------------------*
      *                      * Riciclo su mese successivo              *
      *                      *-----------------------------------------*
           go to     det-mzn-1oe-740.
       det-mzn-1oe-750.
      *              *-------------------------------------------------*
      *              * Riciclo su Next                                 *
      *              *-------------------------------------------------*
           go to     det-mzn-1oe-400.
       det-mzn-1oe-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldi [mmz] tipo conto merce 'N' relative  *
      *    * al 2. esercizio                                           *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-det-mzn-2oe-cri = Credito inizio anno 2. esercizio   *
      *    *                                                           *
      *    *  - w-det-mzn-2oe-tcp = Totale movimenti a nostro credito  *
      *    *                        periodo 2. esercizio               *
      *    *                                                           *
      *    *  - w-det-mzn-2oe-tdp = Totale movimenti a nostro debito   *
      *    *                        periodo 2. esercizio               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-mzn-2oe-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Credito inizio anno 2. esercizio            *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzn-2oe-cri      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti nostro credito periodo 2.  *
      *                  * esercizio                                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzn-2oe-tcp      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti nostro debito periodo 2.   *
      *                  * esercizio                                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzn-2oe-tdp      .
       det-mzn-2oe-050.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per subroutines di scan- *
      *              * sione [mmz] per 2. esercizio                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anno di esercizio da considerare : il 2. e- *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      w-det-mzn-2do-ese    to   w-det-mzn-sub-ese      .
       det-mzn-2oe-100.
      *              *-------------------------------------------------*
      *              * Start su [mmz] per 2. esercizio                 *
      *              *-------------------------------------------------*
           perform   det-mzn-str-000      thru det-mzn-str-999        .
      *              *-------------------------------------------------*
      *              * Se start errata : a correzioni finali           *
      *              *-------------------------------------------------*
           if        w-det-mzn-str-flg    not  = spaces
                     go to det-mzn-2oe-450.
       det-mzn-2oe-150.
      *              *-------------------------------------------------*
      *              * Read next su [mmz] per 2. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mzn-nxt-000      thru det-mzn-nxt-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : a correzioni finali              *
      *              *-------------------------------------------------*
           if        w-det-mzn-nxt-flg    not  = spaces
                     go to det-mzn-2oe-450.
       det-mzn-2oe-200.
      *              *-------------------------------------------------*
      *              * Test max su [mmz] per 2. esercizio              *
      *              *-------------------------------------------------*
           perform   det-mzn-max-000      thru det-mzn-max-999        .
      *              *-------------------------------------------------*
      *              * Se test max non superato : a correzioni finali  *
      *              *-------------------------------------------------*
           if        w-det-mzn-max-flg    not  = spaces
                     go to det-mzn-2oe-450.
       det-mzn-2oe-250.
      *              *-------------------------------------------------*
      *              * Selezioni su [mmz] per 2. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mzn-sel-000      thru det-mzn-sel-999        .
      *              *-------------------------------------------------*
      *              * Se selezioni non superate : riciclo su Next     *
      *              *-------------------------------------------------*
           if        w-det-mzn-sel-flg    not  = spaces
                     go to det-mzn-2oe-150.
       det-mzn-2oe-300.
      *              *-------------------------------------------------*
      *              * Elaborazioni sul record di [mmz] in esame       *
      *              *-------------------------------------------------*
       det-mzn-2oe-310.
      *                  *---------------------------------------------*
      *                  * Sommatoria della quantita' a nostro credito *
      *                  * ad inizio esercizio                         *
      *                  *---------------------------------------------*
           add       rf-mmz-qta-ini       to   w-det-mzn-2oe-cri      .
       det-mzn-2oe-320.
      *                  *---------------------------------------------*
      *                  * Sommatoria movimenti a nostro credito ed a  *
      *                  * nostro debito per il periodo antecedente il *
      *                  * mese di esercizio relativo alla data di ri- *
      *                  * ferimento per determinazione saldi          *
      *                  *---------------------------------------------*
       det-mzn-2oe-325.
      *                      *-----------------------------------------*
      *                      * Se l'indice sul mese e' a zero signifi- *
      *                      * ca che interessa solamente il saldo di  *
      *                      * inizio esercizio, quindi si escludono   *
      *                      * tutte le altre sommatorie               *
      *                      *-----------------------------------------*
           if        w-det-mzn-ism-rif    =    zero
                     go to det-mzn-2oe-400.
       det-mzn-2oe-330.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice su mesi pari    *
      *                      * all'indice su mese di esercizio rela-   *
      *                      * tivo alla data di riferimento per la    *
      *                      * determinazione saldi                    *
      *                      *-----------------------------------------*
           move      w-det-mzn-ism-rif    to   w-det-mzn-inx-mes      .
       det-mzn-2oe-340.
      *                      *-----------------------------------------*
      *                      * Decremento indice su mesi               *
      *                      *-----------------------------------------*
           subtract  1                    from w-det-mzn-inx-mes      .
      *                      *-----------------------------------------*
      *                      * Se giunti a zero : fine ciclo           *
      *                      *-----------------------------------------*
           if        w-det-mzn-inx-mes    =    zero
                     go to det-mzn-2oe-350.
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro credito   *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-cre
                    (w-det-mzn-inx-mes)   to   w-det-mzn-2oe-tcp      .
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro debito    *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-deb
                    (w-det-mzn-inx-mes)   to   w-det-mzn-2oe-tdp      .
      *                      *-----------------------------------------*
      *                      * Riciclo su mese precedente              *
      *                      *-----------------------------------------*
           go to     det-mzn-2oe-340.
       det-mzn-2oe-350.
      *                  *---------------------------------------------*
      *                  * Se la data di riferimento corrisponde alla  *
      *                  * data attuale oppure se la data di riferi-   *
      *                  * mento corrisponde ad un fine mese, si ac-   *
      *                  * cumulano anche i movimenti a credito ed a   *
      *                  * debito relativi al mese di esercizio rela-  *
      *                  * tivo alla data di riferimento per determi-  *
      *                  * nazione saldi                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-det-mzn-dat-rif    not  = w-det-mzn-dat-att and
                     w-det-mzn-fin-mes    not  = "F"
                     go to det-mzn-2oe-400.
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro credito   *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-cre
                    (w-det-mzn-ism-rif)   to   w-det-mzn-2oe-tcp      .
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro debito    *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-deb
                    (w-det-mzn-ism-rif)   to   w-det-mzn-2oe-tdp      .
       det-mzn-2oe-400.
      *              *-------------------------------------------------*
      *              * Riciclo su Next                                 *
      *              *-------------------------------------------------*
           go to     det-mzn-2oe-150.
       det-mzn-2oe-450.
      *              *-------------------------------------------------*
      *              * Correzioni finali                               *
      *              *-------------------------------------------------*
       det-mzn-2oe-500.
      *                  *---------------------------------------------*
      *                  * Se l'indice sul mese e' a zero significa    *
      *                  * che interessa solamente il saldo di inizio  *
      *                  * esercizio, quindi si esce immediatamente    *
      *                  * senza effettuare le correzioni finali, in   *
      *                  * quanto non necessarie                       *
      *                  *---------------------------------------------*
           if        w-det-mzn-ism-rif    =    zero
                     go to det-mzn-2oe-999.
      *                  *---------------------------------------------*
      *                  * Se la data di riferimento corrisponde alla  *
      *                  * data attuale oppure se la data di riferi-   *
      *                  * mento corrisponde ad un fine mese, si esce  *
      *                  * immediatamente senza effettuare le corre-   *
      *                  * zioni finali, in quanto non necessarie      *
      *                  *---------------------------------------------*
           if        w-det-mzn-dat-rif    =    w-det-mzn-dat-att or
                     w-det-mzn-fin-mes    =    "F"
                     go to det-mzn-2oe-999.
      *                  *---------------------------------------------*
      *                  * Preparazione della data relativa al primo   *
      *                  * giorno del mese della della data di rife-   *
      *                  * rimento                                     *
      *                  *---------------------------------------------*
           move      w-det-mzn-dat-rif    to   s-dat                  .
           move      01                   to   s-gio                  .
           move      s-dat                to   w-det-mzn-g1m-ddr      .
       det-mzn-2oe-550.
      *                  *---------------------------------------------*
      *                  * Start su [mmr]                              *
      *                  *---------------------------------------------*
       det-mzn-2oe-555.
      *                      *-----------------------------------------*
      *                      * Deviazione se una o tutte le dipendenze *
      *                      *-----------------------------------------*
           if        w-det-mzn-uot-dpz    =    "T"
                     go to det-mzn-2oe-565.
       det-mzn-2oe-560.
      *                      *-----------------------------------------*
      *                      * Se una sola dipendenza                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPZMAG    "         to   f-key                  .
           move      w-det-mzn-cod-dpz    to   rf-mmr-cod-dpz         .
           move      w-det-mzn-tip-mag    to   rf-mmr-tip-mag         .
           move      w-det-mzn-num-mag    to   rf-mmr-num-mag         .
           move      w-det-mzn-g1m-ddr    to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                          *-------------------------------------*
      *                          * A controllo esito Start             *
      *                          *-------------------------------------*
           go to     det-mzn-2oe-570.
       det-mzn-2oe-565.
      *                      *-----------------------------------------*
      *                      * Se tutte le dipendenze                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      w-det-mzn-tip-mag    to   rf-mmr-tip-mag         .
           move      w-det-mzn-num-mag    to   rf-mmr-num-mag         .
           move      w-det-mzn-g1m-ddr    to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                          *-------------------------------------*
      *                          * A controllo esito Start             *
      *                          *-------------------------------------*
           go to     det-mzn-2oe-570.
       det-mzn-2oe-570.
      *                      *-----------------------------------------*
      *                      * Se Start errata : uscita                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-mzn-2oe-999.
       det-mzn-2oe-600.
      *                  *---------------------------------------------*
      *                  * Read Next su [mmr]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-mzn-2oe-999.
       det-mzn-2oe-650.
      *                  *---------------------------------------------*
      *                  * Test Max su [mmr], se non superato : uscita *
      *                  *---------------------------------------------*
       det-mzn-2oe-655.
      *                      *-----------------------------------------*
      *                      * Deviazione se una o tutte le dipendenze *
      *                      *-----------------------------------------*
           if        w-det-mzn-uot-dpz    =    "T"
                     go to det-mzn-2oe-665.
       det-mzn-2oe-660.
      *                      *-----------------------------------------*
      *                      * Se una sola dipendenza                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Max su codice dipendenza            *
      *                          *-------------------------------------*
           if        rf-mmr-cod-dpz       not  = w-det-mzn-cod-dpz
                     go to det-mzn-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su tipo codice magazzino        *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mag       not  = w-det-mzn-tip-mag
                     go to det-mzn-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su codice numerico magazzino    *
      *                          *-------------------------------------*
           if        rf-mmr-num-mag       not  = w-det-mzn-num-mag
                     go to det-mzn-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su data registrazione           *
      *                          *-------------------------------------*
           if        rf-mmr-dat-reg       >    w-det-mzn-dat-rif
                     go to det-mzn-2oe-999.
      *                          *-------------------------------------*
      *                          * Test max superato                   *
      *                          *-------------------------------------*
           go to     det-mzn-2oe-700.
       det-mzn-2oe-665.
      *                      *-----------------------------------------*
      *                      * Se tutte le dipendenze                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Max su tipo codice magazzino        *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mag       not  = w-det-mzn-tip-mag
                     go to det-mzn-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su codice numerico magazzino    *
      *                          *-------------------------------------*
           if        rf-mmr-num-mag       not  = w-det-mzn-num-mag
                     go to det-mzn-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su data registrazione           *
      *                          *-------------------------------------*
           if        rf-mmr-dat-reg       >    w-det-mzn-dat-rif
                     go to det-mzn-2oe-999.
      *                          *-------------------------------------*
      *                          * Test max superato                   *
      *                          *-------------------------------------*
           go to     det-mzn-2oe-700.
       det-mzn-2oe-700.
      *                  *---------------------------------------------*
      *                  * Selezione su [mmr], se non superata : rici- *
      *                  * clo a lettura                               *
      *                  *---------------------------------------------*
       det-mzn-2oe-705.
      *                      *-----------------------------------------*
      *                      * Sel su data registrazione               *
      *                      *                                         *
      *                      * Se non compresa tra il primo giorno del *
      *                      * mese relativo alla data di riferimento, *
      *                      * e la data di riferimento stessa : sele- *
      *                      * zione non superata                      *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-dat-reg       <    w-det-mzn-g1m-ddr or
                     rf-mmr-dat-reg       >    w-det-mzn-dat-rif
                     go to det-mzn-2oe-600.
       det-mzn-2oe-710.
      *                      *-----------------------------------------*
      *                      * Sel su codice dipendenza                *
      *                      *                                         *
      *                      * Se una sola dipendenza, ma la dipenden- *
      *                      * za del movimento letto non corrisponde  *
      *                      * a quella richiesta : selezione non su-  *
      *                      * perata                                  *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzn-uot-dpz    not  = "T"
                     if    rf-mmr-cod-dpz not  = w-det-mzn-cod-dpz
                           go to  det-mzn-2oe-600.
       det-mzn-2oe-715.
      *                      *-----------------------------------------*
      *                      * Sel su tipo di trattamento per il valo- *
      *                      * re per la causale                       *
      *                      *                                         *
      *                      * Se movimento di solo valore : selezione *
      *                      * non superata                            *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-val       =    "C" or
                     rf-mmr-trt-val       =    "X" or
                     rf-mmr-trt-val       =    "R" or
                     rf-mmr-trt-val       =    "Y"
                     go to det-mzn-2oe-600.
       det-mzn-2oe-720.
      *                      *-----------------------------------------*
      *                      * Sel su tipo movimentazione di magazzino *
      *                      *                                         *
      *                      * Se movimento di carico o scarico : se-  *
      *                      * lezione superata                        *
      *                      *                                         *
      *                      * Se movimentazione interna : selezione   *
      *                      * non superata                            *
      *                      *                                         *
      *                      * Se tipo movimento non riconosciuto :    *
      *                      * selezione non superata                  *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mdm       =    01 or
                     rf-mmr-tip-mdm       =    02
                     go to det-mzn-2oe-725
           else if   rf-mmr-tip-mdm       =    03
                     go to det-mzn-2oe-600
           else      go to det-mzn-2oe-600.
       det-mzn-2oe-725.
      *                      *-----------------------------------------*
      *                      * Sel su trattamento conto merce e codice *
      *                      * conto merce                             *
      *                      *                                         *
      *                      * Se il tipo trattamento merce in conto   *
      *                      * indica che il movimento puo' riferir-   *
      *                      * si, anche se non necessariamente, ad    *
      *                      * un precedente movimento di merce in     *
      *                      * conto                                   *
      *                      *                                         *
      *                      *    Se non e' stato citato alcun conto   *
      *                      *    merce : selezione non superata       *
      *                      *                                         *
      *                      *    Altrimenti : selezione superata      *
      *                      *                                         *
      *                      * Se il movimento non puo' riferirsi ad   *
      *                      * un altro documento : selezione non su-  *
      *                      * perata                                  *
      *                      *                                         *
      *                      * Se il movimento stesso e' una movimen-  *
      *                      * tazione di merce in conto : selezione   *
      *                      * superata                                *
      *                      *                                         *
      *                      * Altrimenti : selezione non superata     *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-mic       =    01
                     if    rf-mmr-cod-ctm =    spaces
                           go to det-mzn-2oe-600
                     else  go to det-mzn-2oe-730
           else if   rf-mmr-trt-mic       =    02
                     go to det-mzn-2oe-600
           else if   rf-mmr-trt-mic       =    03
                     go to det-mzn-2oe-730
           else      go to det-mzn-2oe-600.
       det-mzn-2oe-730.
      *                      *-----------------------------------------*
      *                      * Sel su tipo conto merce                 *
      *                      *                                         *
      *                      * Se diverso da 'N' : selezione non su-   *
      *                      * perata                                  *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mic       not  = "N"
                     go to det-mzn-2oe-600.
       det-mzn-2oe-735.
      *                      *-----------------------------------------*
      *                      * Sel su codice conto merce               *
      *                      *                                         *
      *                      * Se a spaces : selezione non superata    *
      *                      *                                         *
      *                      * Se un solo conto merce, ma il conto     *
      *                      * merce del movimento letto non corri-    *
      *                      * sponde a quello richiesto : selezio-    *
      *                      * ne non superata                         *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-cod-ctm       =    spaces
                     go to det-mzn-2oe-600.
           if        w-det-mzn-uot-ctm    not  = "T"
                     if    rf-mmr-cod-ctm not  = w-det-mzn-cod-ctm
                           go to  det-mzn-2oe-600.
       det-mzn-2oe-740.
      *                      *-----------------------------------------*
      *                      * Sel su tipo archivio                    *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i tipi ar- *
      *                      * chivio : selezione superata             *
      *                      *                                         *
      *                      * Se un solo tipo archivio, ma il tipo    *
      *                      * archivio del movimento letto non cor-   *
      *                      * risponde a quello richiesto : sele-     *
      *                      * zione non superata                      *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-2oe-745.
           if        rf-mmr-tip-arc       not  = w-det-mzn-tip-arc
                     go to det-mzn-2oe-600.
       det-mzn-2oe-745.
      *                      *-----------------------------------------*
      *                      * Sel su codice archivio                  *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i tipi ar- *
      *                      * chivio : selezione superata             *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i codici   *
      *                      * archivio : selezione superata           *
      *                      *                                         *
      *                      * Se un solo codice archivio, ma il co-   *
      *                      * dice archivio del movimento letto non   *
      *                      * corrisponde a quello richiesto : sele-  *
      *                      * zione non superata                      *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-2oe-750.
           if        w-det-mzn-uot-arc    =    "T"
                     go to det-mzn-2oe-750.
           if        rf-mmr-cod-arc       not  = w-det-mzn-cod-arc
                     go to det-mzn-2oe-600.
       det-mzn-2oe-750.
      *                      *-----------------------------------------*
      *                      * Sel su codice dipendenza per il codice  *
      *                      * archivio                                *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i tipi ar- *
      *                      * chivio : selezione superata             *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i codici   *
      *                      * archivio : selezione superata           *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i codici   *
      *                      * dipendenza per il codice archivio : se- *
      *                      * lezione superata                        *
      *                      *                                         *
      *                      * Se un solo codice dipendenza per il co- *
      *                      * dice archivio, ma il codice dipendenza  *
      *                      * per il codice archivio del movimento    *
      *                      * letto non corrisponde a quello richie-  *
      *                      * sto : selezione non superata            *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-2oe-755.
           if        w-det-mzn-uot-arc    =    "T"
                     go to det-mzn-2oe-755.
           if        w-det-mzn-uot-dpa    =    "T"
                     go to det-mzn-2oe-755.
           if        rf-mmr-dpz-arc       not  = w-det-mzn-dpz-arc
                     go to det-mzn-2oe-600.
       det-mzn-2oe-755.
      *                      *-----------------------------------------*
      *                      * Sel su tipo codice magazzino            *
      *                      *                                         *
      *                      * Se non corrisponde a quello richiesto : *
      *                      * selezione non superata                  *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mag       not  = w-det-mzn-tip-mag
                     go to det-mzn-2oe-600.
       det-mzn-2oe-760.
      *                      *-----------------------------------------*
      *                      * Sel su codice numerico magazzino        *
      *                      *                                         *
      *                      * Se non corrisponde a quello richiesto : *
      *                      * selezione non superata                  *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-num-mag       not  = w-det-mzn-num-mag
                     go to det-mzn-2oe-600.
       det-mzn-2oe-765.
      *                      *-----------------------------------------*
      *                      * Sel su variante al codice magazzino     *
      *                      *                                         *
      *                      * Se tutte le varianti : selezione supe-  *
      *                      * rata                                    *
      *                      *                                         *
      *                      * Se una sola variante, ma il codice va-  *
      *                      * riante del movimento letto non corri-   *
      *                      * sponde a quello richiesto : selezione   *
      *                      * non superata                            *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-2oe-770.
           if        rf-mmr-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-2oe-600.
       det-mzn-2oe-770.
      *                      *-----------------------------------------*
      *                      * Selezione su [mmr] superata             *
      *                      *-----------------------------------------*
           go to     det-mzn-2oe-800.
       det-mzn-2oe-800.
      *                  *---------------------------------------------*
      *                  * Elaborazioni sul record di [mmr] in esame : *
      *                  * simulazione di aggiornamento                *
      *                  *---------------------------------------------*
       det-mzn-2oe-825.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se si deve aggior- *
      *                      * nare il credito o il debito.            *
      *                      *                                         *
      *                      * Se il tipo trattamento merce in conto   *
      *                      * indica che il movimento puo' riferir-   *
      *                      * si, anche se non necessariamente, ad    *
      *                      * un precedente movimento di merce in     *
      *                      * conto :                                 *
      *                      *                                         *
      *                      *    Se movimento di carico si aggiorna   *
      *                      *    il progressivo credito               *
      *                      *                                         *
      *                      *    Se movimento di scarico si aggiorna  *
      *                      *    il progressivo debito                *
      *                      *                                         *
      *                      *    Altrimenti nessun aggiornamento      *
      *                      *                                         *
      *                      * Se il movimento stesso e' una movimen-  *
      *                      * tazione di merce in conto :             *
      *                      *                                         *
      *                      *    Se movimento di carico si aggiorna   *
      *                      *    il progressivo debito                *
      *                      *                                         *
      *                      *    Se movimento di scarico si aggiorna  *
      *                      *    il progressivo credito               *
      *                      *                                         *
      *                      *    Altrimenti nessun aggiornamento      *
      *                      *                                         *
      *                      * Altrimenti nessun aggiornamento         *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-mic       =    01
                     if      rf-mmr-tip-mdm
                                          =    01
                             go to det-mzn-2oe-850
                     else if rf-mmr-tip-mdm
                                          =    02
                             go to det-mzn-2oe-875
                     else    go to det-mzn-2oe-900
           else if   rf-mmr-trt-mic       =    03
                     if      rf-mmr-tip-mdm
                                          =    01
                             go to det-mzn-2oe-875
                     else if rf-mmr-tip-mdm
                                          =    02
                             go to det-mzn-2oe-850
                     else    go to det-mzn-2oe-900
           else      go to   det-mzn-2oe-900.
       det-mzn-2oe-850.
      *                      *-----------------------------------------*
      *                      * Aggiornamento progressivo credito       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Sommatoria progressivo credito      *
      *                          *-------------------------------------*
           add       rf-mmr-qta-mov       to   w-det-mzn-2oe-tcp      .
      *                          *-------------------------------------*
      *                          * A fine elaborazione su record [mmz] *
      *                          * in esame                            *
      *                          *-------------------------------------*
           go to     det-mzn-2oe-900.
       det-mzn-2oe-875.
      *                      *-----------------------------------------*
      *                      * Aggiornamento progressivo debito        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Sommatoria progressivo debito       *
      *                          *-------------------------------------*
           add       rf-mmr-qta-mov       to   w-det-mzn-2oe-tdp      .
      *                          *-------------------------------------*
      *                          * A fine elaborazione su record [mmz] *
      *                          * in esame                            *
      *                          *-------------------------------------*
           go to     det-mzn-2oe-900.
       det-mzn-2oe-900.
      *                      *-----------------------------------------*
      *                      * Fine elaborazione su record [mmz] in e- *
      *                      * same                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Riciclo su Read Next su [mmr]       *
      *                          *-------------------------------------*
           go to     det-mzn-2oe-600.
       det-mzn-2oe-999.
           exit.

      *    *===========================================================*
      *    * Start su [mmz] per determinazione saldi da [mmz]          *
      *    *-----------------------------------------------------------*
       det-mzn-str-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mzn-str-flg      .
       det-mzn-str-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se una o tutte le dipen-   *
      *              * denze                                           *
      *              *-------------------------------------------------*
           if        w-det-mzn-uot-dpz    =    "T"
                     go to det-mzn-str-500.
       det-mzn-str-200.
      *              *-------------------------------------------------*
      *              * Se una sola dipendenza                          *
      *              *-------------------------------------------------*
       det-mzn-str-250.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se un solo tipo archi- *
      *                  * vio o tutti i tipi archivio                 *
      *                  *---------------------------------------------*
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-str-450.
       det-mzn-str-300.
      *                  *---------------------------------------------*
      *                  * Se un solo tipo archivio                    *
      *                  *---------------------------------------------*
       det-mzn-str-325.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se uno o tutti i   *
      *                      * codici archivio                         *
      *                      *-----------------------------------------*
           if        w-det-mzn-uot-arc    =    "T"
                     go to det-mzn-str-400.
       det-mzn-str-350.
      *                      *-----------------------------------------*
      *                      * Se un solo codice archivio              *
      *                      *-----------------------------------------*
       det-mzn-str-375.
      *                          *-------------------------------------*
      *                          * Start per chiave 'ARCMAG'           *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ARCMAG    "         to   f-key                  .
           move      w-det-mzn-sub-ese    to   rf-mmz-ann-ese         .
           move      w-det-mzn-cod-dpz    to   rf-mmz-cod-dpz         .
           move      w-det-mzn-tip-arc    to   rf-mmz-tip-arc         .
           move      w-det-mzn-cod-arc    to   rf-mmz-cod-arc         .
           if        w-det-mzn-uot-dpa    =    "T"
                     move  spaces         to   rf-mmz-dpz-arc
           else      move  w-det-mzn-dpz-arc
                                          to   rf-mmz-dpz-arc         .
           move      w-det-mzn-tip-mag    to   rf-mmz-tip-mag         .
           move      w-det-mzn-num-mag    to   rf-mmz-num-mag         .
           if        w-det-mzn-uot-var    =    "T"
                     move  spaces         to   rf-mmz-var-mag
           else      move  w-det-mzn-var-mag
                                          to   rf-mmz-var-mag         .
           move      "N"                  to   rf-mmz-tip-ctm         .
           if        w-det-mzn-uot-ctm    =    "T"
                     move  spaces         to   rf-mmz-cod-ctm
           else      move  w-det-mzn-cod-ctm
                                          to   rf-mmz-cod-ctm         .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzn-str-380.
      *                          *-------------------------------------*
      *                          * Se start errata : uscita con flag   *
      *                          * di errore                           *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzn-str-flg
                     go to det-mzn-str-999.
       det-mzn-str-385.
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-mzn-str-999.
       det-mzn-str-400.
      *                      *-----------------------------------------*
      *                      * Se tutti i codici archivio              *
      *                      *-----------------------------------------*
       det-mzn-str-405.
      *                          *-------------------------------------*
      *                          * Start per chiave 'MAGARC'           *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGARC    "         to   f-key                  .
           move      w-det-mzn-sub-ese    to   rf-mmz-ann-ese         .
           move      w-det-mzn-cod-dpz    to   rf-mmz-cod-dpz         .
           move      w-det-mzn-tip-mag    to   rf-mmz-tip-mag         .
           move      w-det-mzn-num-mag    to   rf-mmz-num-mag         .
           if        w-det-mzn-uot-var    =    "T"
                     move  spaces         to   rf-mmz-var-mag
           else      move  w-det-mzn-var-mag
                                          to   rf-mmz-var-mag         .
           move      "N"                  to   rf-mmz-tip-ctm         .
           if        w-det-mzn-uot-ctm    =    "T"
                     move  spaces         to   rf-mmz-cod-ctm
           else      move  w-det-mzn-cod-ctm
                                          to   rf-mmz-cod-ctm         .
           move      w-det-mzn-tip-arc    to   rf-mmz-tip-arc         .
           move      zero                 to   rf-mmz-cod-arc         .
           move      spaces               to   rf-mmz-dpz-arc         .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzn-str-410.
      *                          *-------------------------------------*
      *                          * Se start errata : uscita con flag   *
      *                          * di errore                           *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzn-str-flg
                     go to det-mzn-str-999.
       det-mzn-str-415.
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-mzn-str-999.
       det-mzn-str-450.
      *                  *---------------------------------------------*
      *                  * Se tutti i tipi archivio                    *
      *                  *---------------------------------------------*
       det-mzn-str-455.
      *                      *-----------------------------------------*
      *                      * Start per chiave 'MAGARC'               *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGARC    "         to   f-key                  .
           move      w-det-mzn-sub-ese    to   rf-mmz-ann-ese         .
           move      w-det-mzn-cod-dpz    to   rf-mmz-cod-dpz         .
           move      w-det-mzn-tip-mag    to   rf-mmz-tip-mag         .
           move      w-det-mzn-num-mag    to   rf-mmz-num-mag         .
           if        w-det-mzn-uot-var    =    "T"
                     move  spaces         to   rf-mmz-var-mag
           else      move  w-det-mzn-var-mag
                                          to   rf-mmz-var-mag         .
           move      "N"                  to   rf-mmz-tip-ctm         .
           if        w-det-mzn-uot-ctm    =    "T"
                     move  spaces         to   rf-mmz-cod-ctm
           else      move  w-det-mzn-cod-ctm
                                          to   rf-mmz-cod-ctm         .
           move      spaces               to   rf-mmz-tip-arc         .
           move      zero                 to   rf-mmz-cod-arc         .
           move      spaces               to   rf-mmz-dpz-arc         .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzn-str-460.
      *                      *-----------------------------------------*
      *                      * Se start errata : uscita con flag di    *
      *                      * errore                                  *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzn-str-flg
                     go to det-mzn-str-999.
       det-mzn-str-465.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-mzn-str-999.
       det-mzn-str-500.
      *              *-------------------------------------------------*
      *              * Se tutte le dipendenze                          *
      *              *-------------------------------------------------*
       det-mzn-str-505.
      *                  *---------------------------------------------*
      *                  * Start per chiave 'MGCMAR'                   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MGCMAR    "         to   f-key                  .
           move      w-det-mzn-sub-ese    to   rf-mmz-ann-ese         .
           move      w-det-mzn-tip-mag    to   rf-mmz-tip-mag         .
           move      w-det-mzn-num-mag    to   rf-mmz-num-mag         .
           if        w-det-mzn-uot-var    =    "T"
                     move  spaces         to   rf-mmz-var-mag
           else      move  w-det-mzn-var-mag
                                          to   rf-mmz-var-mag         .
           move      "N"                  to   rf-mmz-tip-ctm         .
           if        w-det-mzn-uot-ctm    =    "T"
                     move  spaces         to   rf-mmz-cod-ctm
           else      move  w-det-mzn-cod-ctm
                                          to   rf-mmz-cod-ctm         .
           if        w-det-mzn-tip-arc    =    spaces
                     move  spaces         to   rf-mmz-tip-arc
                     move  zero           to   rf-mmz-cod-arc
                     move  spaces         to   rf-mmz-dpz-arc
           else if   w-det-mzn-uot-arc    =    "T"
                     move  w-det-mzn-tip-arc
                                          to   rf-mmz-tip-arc
                     move  zero           to   rf-mmz-cod-arc
                     move  spaces         to   rf-mmz-dpz-arc
           else      move  w-det-mzn-tip-arc
                                          to   rf-mmz-tip-arc
                     move  w-det-mzn-cod-arc
                                          to   rf-mmz-cod-arc
                     if    w-det-mzn-uot-dpa
                                          =    "T"
                           move  spaces   to   rf-mmz-dpz-arc
                     else  move  w-det-mzn-dpz-arc
                                          to   rf-mmz-dpz-arc         .
           move      zero                 to   rf-mmz-cod-dpz         .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzn-str-415.
      *                  *---------------------------------------------*
      *                  * Se start errata : uscita con flag di errore *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzn-str-flg
                     go to det-mzn-str-999.
       det-mzn-str-420.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-mzn-str-999.
       det-mzn-str-999.
           exit.

      *    *===========================================================*
      *    * Read next su [mmz] per determinazione saldi da [mmz]      *
      *    *-----------------------------------------------------------*
       det-mzn-nxt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mzn-nxt-flg      .
       det-mzn-nxt-100.
      *              *-------------------------------------------------*
      *              * Read Next                                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzn-nxt-200.
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag di errore        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzn-nxt-flg
                     go to det-mzn-nxt-999.
       det-mzn-nxt-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-mzn-nxt-999.
       det-mzn-nxt-999.
           exit.

      *    *===========================================================*
      *    * Test max su [mmz] per determinazione saldi da [mmz]       *
      *    *-----------------------------------------------------------*
       det-mzn-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mzn-max-flg      .
       det-mzn-max-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se una o tutte le dipen-   *
      *              * denze                                           *
      *              *-------------------------------------------------*
           if        w-det-mzn-uot-dpz    =    "T"
                     go to det-mzn-max-600.
       det-mzn-max-200.
      *              *-------------------------------------------------*
      *              * Se una sola dipendenza                          *
      *              *-------------------------------------------------*
       det-mzn-max-250.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se un solo tipo archi- *
      *                  * vio o tutti i tipi archivio                 *
      *                  *---------------------------------------------*
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-max-500.
       det-mzn-max-300.
      *                  *---------------------------------------------*
      *                  * Se un solo tipo archivio                    *
      *                  *---------------------------------------------*
       det-mzn-max-325.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se uno o tutti i   *
      *                      * codici archivio                         *
      *                      *-----------------------------------------*
           if        w-det-mzn-uot-arc    =    "T"
                     go to det-mzn-max-450.
       det-mzn-max-350.
      *                      *-----------------------------------------*
      *                      * Se un solo codice archivio              *
      *                      *-----------------------------------------*
       det-mzn-max-375.
      *                          *-------------------------------------*
      *                          * Test su anno di esercizio           *
      *                          *-------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzn-sub-ese
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-380.
      *                          *-------------------------------------*
      *                          * Test su codice dipendenza           *
      *                          *-------------------------------------*
           if        rf-mmz-cod-dpz       not  = w-det-mzn-cod-dpz
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-385.
      *                          *-------------------------------------*
      *                          * Test su tipo archivio               *
      *                          *-------------------------------------*
           if        rf-mmz-tip-arc       not  = w-det-mzn-tip-arc
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-390.
      *                          *-------------------------------------*
      *                          * Test su codice archivio             *
      *                          *-------------------------------------*
           if        rf-mmz-cod-arc       not  = w-det-mzn-cod-arc
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-395.
      *                          *-------------------------------------*
      *                          * Test su codice dipendenza per il    *
      *                          * codice archivio                     *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-dpa    =    "T"
                     go to det-mzn-max-400.
           if        rf-mmz-dpz-arc       not  = w-det-mzn-dpz-arc
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-400.
      *                          *-------------------------------------*
      *                          * Test su tipo codice di magazzino    *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-dpa    =    "T"
                     go to det-mzn-max-405.
           if        rf-mmz-dpz-arc       not  = w-det-mzn-dpz-arc
                     go to det-mzn-max-405.
           if        rf-mmz-tip-mag       not  = w-det-mzn-tip-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-405.
      *                          *-------------------------------------*
      *                          * Test su codice di magazzino         *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-dpa    =    "T"
                     go to det-mzn-max-410.
           if        rf-mmz-dpz-arc       not  = w-det-mzn-dpz-arc
                     go to det-mzn-max-410.
           if        rf-mmz-tip-mag       not  = w-det-mzn-tip-mag
                     go to det-mzn-max-410.
           if        rf-mmz-num-mag       not  = w-det-mzn-num-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-410.
      *                          *-------------------------------------*
      *                          * Test su codice variante             *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-dpa    =    "T"
                     go to det-mzn-max-415.
           if        rf-mmz-dpz-arc       not  = w-det-mzn-dpz-arc
                     go to det-mzn-max-415.
           if        rf-mmz-tip-mag       not  = w-det-mzn-tip-mag
                     go to det-mzn-max-415.
           if        rf-mmz-num-mag       not  = w-det-mzn-num-mag
                     go to det-mzn-max-415.
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-415.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-415.
      *                          *-------------------------------------*
      *                          * Test su tipo conto merce            *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-dpa    =    "T"
                     go to det-mzn-max-420.
           if        rf-mmz-dpz-arc       not  = w-det-mzn-dpz-arc
                     go to det-mzn-max-420.
           if        rf-mmz-tip-mag       not  = w-det-mzn-tip-mag
                     go to det-mzn-max-420.
           if        rf-mmz-num-mag       not  = w-det-mzn-num-mag
                     go to det-mzn-max-420.
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-420.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-420.
           if        rf-mmz-tip-ctm       not  = "N"
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-420.
      *                          *-------------------------------------*
      *                          * Test su codice conto merce          *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-dpa    =    "T"
                     go to det-mzn-max-425.
           if        rf-mmz-dpz-arc       not  = w-det-mzn-dpz-arc
                     go to det-mzn-max-425.
           if        rf-mmz-tip-mag       not  = w-det-mzn-tip-mag
                     go to det-mzn-max-425.
           if        rf-mmz-num-mag       not  = w-det-mzn-num-mag
                     go to det-mzn-max-425.
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-425.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-425.
           if        rf-mmz-tip-ctm       not  = "N"
                     go to det-mzn-max-425.
           if        w-det-mzn-uot-ctm    =    "T"
                     go to det-mzn-max-425.
           if        rf-mmz-cod-ctm       not  = w-det-mzn-cod-ctm
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-425.
      *                          *-------------------------------------*
      *                          * Uscita per test superato            *
      *                          *-------------------------------------*
           go to     det-mzn-max-999.
       det-mzn-max-450.
      *                      *-----------------------------------------*
      *                      * Se tutti i codici archivio              *
      *                      *-----------------------------------------*
       det-mzn-max-455.
      *                          *-------------------------------------*
      *                          * Test su anno di esercizio           *
      *                          *-------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzn-sub-ese
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-460.
      *                          *-------------------------------------*
      *                          * Test su codice dipendenza           *
      *                          *-------------------------------------*
           if        rf-mmz-cod-dpz       not  = w-det-mzn-cod-dpz
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-465.
      *                          *-------------------------------------*
      *                          * Test su tipo codice di magazzino    *
      *                          *-------------------------------------*
           if        rf-mmz-tip-mag       not  = w-det-mzn-tip-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-470.
      *                          *-------------------------------------*
      *                          * Test su codice di magazzino         *
      *                          *-------------------------------------*
           if        rf-mmz-num-mag       not  = w-det-mzn-num-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-475.
      *                          *-------------------------------------*
      *                          * Test su codice variante             *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-480.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-480.
      *                          *-------------------------------------*
      *                          * Test su tipo conto merce            *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-485.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-485.
           if        rf-mmz-tip-ctm       not  = "N"
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-485.
      *                          *-------------------------------------*
      *                          * Test su codice conto merce          *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-490.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-490.
           if        rf-mmz-tip-ctm       not  = "N"
                     go to det-mzn-max-490.
           if        w-det-mzn-uot-ctm    =    "T"
                     go to det-mzn-max-490.
           if        rf-mmz-cod-ctm       not  = w-det-mzn-cod-ctm
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-490.
      *                          *-------------------------------------*
      *                          * Test su tipo archivio               *
      *                          *-------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-495.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-495.
           if        rf-mmz-tip-ctm       not  = "N"
                     go to det-mzn-max-495.
           if        w-det-mzn-uot-ctm    =    "T"
                     go to det-mzn-max-495.
           if        rf-mmz-cod-ctm       not  = w-det-mzn-cod-ctm
                     go to det-mzn-max-495.
           if        rf-mmz-tip-arc       not  = w-det-mzn-tip-arc
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-495.
      *                          *-------------------------------------*
      *                          * Uscita per test superato            *
      *                          *-------------------------------------*
           go to     det-mzn-max-999.
       det-mzn-max-500.
      *                  *---------------------------------------------*
      *                  * Se tutti i tipo archivio                    *
      *                  *---------------------------------------------*
       det-mzn-max-505.
      *                      *-----------------------------------------*
      *                      * Test su anno di esercizio               *
      *                      *-----------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzn-sub-ese
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-510.
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza               *
      *                      *-----------------------------------------*
           if        rf-mmz-cod-dpz       not  = w-det-mzn-cod-dpz
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-515.
      *                      *-----------------------------------------*
      *                      * Test su tipo codice di magazzino        *
      *                      *-----------------------------------------*
           if        rf-mmz-tip-mag       not  = w-det-mzn-tip-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-520.
      *                      *-----------------------------------------*
      *                      * Test su codice di magazzino             *
      *                      *-----------------------------------------*
           if        rf-mmz-num-mag       not  = w-det-mzn-num-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-525.
      *                      *-----------------------------------------*
      *                      * Test su codice variante                 *
      *                      *-----------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-530.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-530.
      *                      *-----------------------------------------*
      *                      * Test su tipo conto merce                *
      *                      *-----------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-535.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-535.
           if        rf-mmz-tip-ctm       not  = "N"
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-535.
      *                      *-----------------------------------------*
      *                      * Test su codice conto merce              *
      *                      *-----------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-540.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-540.
           if        rf-mmz-tip-ctm       not  = "N"
                     go to det-mzn-max-540.
           if        w-det-mzn-uot-ctm    =    "T"
                     go to det-mzn-max-540.
           if        rf-mmz-cod-ctm       not  = w-det-mzn-cod-ctm
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-540.
      *                      *-----------------------------------------*
      *                      * Uscita per test superato                *
      *                      *-----------------------------------------*
           go to     det-mzn-max-999.
       det-mzn-max-600.
      *              *-------------------------------------------------*
      *              * Se tutte le dipendenza                          *
      *              *-------------------------------------------------*
       det-mzn-max-605.
      *                  *---------------------------------------------*
      *                  * Test su anno di esercizio                   *
      *                  *---------------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzn-sub-ese
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-610.
      *                  *---------------------------------------------*
      *                  * Test su tipo codice di magazzino            *
      *                  *---------------------------------------------*
           if        rf-mmz-tip-mag       not  = w-det-mzn-tip-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-615.
      *                  *---------------------------------------------*
      *                  * Test su codice di magazzino                 *
      *                  *---------------------------------------------*
           if        rf-mmz-num-mag       not  = w-det-mzn-num-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-620.
      *                  *---------------------------------------------*
      *                  * Test su codice variante                     *
      *                  *---------------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-625.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-625.
      *                  *---------------------------------------------*
      *                  * Test su tipo conto merce                    *
      *                  *---------------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-630.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-630.
           if        rf-mmz-tip-ctm       not  = "N"
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-630.
      *                  *---------------------------------------------*
      *                  * Test su codice conto merce                  *
      *                  *---------------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-635.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-635.
           if        rf-mmz-tip-ctm       not  = "N"
                     go to det-mzn-max-635.
           if        w-det-mzn-uot-ctm    =    "T"
                     go to det-mzn-max-635.
           if        rf-mmz-cod-ctm       not  = w-det-mzn-cod-ctm
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-635.
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-640.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-640.
           if        rf-mmz-tip-ctm       not  = "N"
                     go to det-mzn-max-640.
           if        w-det-mzn-uot-ctm    =    "T"
                     go to det-mzn-max-640.
           if        rf-mmz-cod-ctm       not  = w-det-mzn-cod-ctm
                     go to det-mzn-max-640.
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-max-640.
           if        rf-mmz-tip-arc       not  = w-det-mzn-tip-arc
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-640.
      *                  *---------------------------------------------*
      *                  * Test su codice archivio                     *
      *                  *---------------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-645.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-645.
           if        rf-mmz-tip-ctm       not  = "N"
                     go to det-mzn-max-645.
           if        w-det-mzn-uot-ctm    =    "T"
                     go to det-mzn-max-645.
           if        rf-mmz-cod-ctm       not  = w-det-mzn-cod-ctm
                     go to det-mzn-max-645.
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-max-645.
           if        rf-mmz-tip-arc       not  = w-det-mzn-tip-arc
                     go to det-mzn-max-645.
           if        w-det-mzn-uot-arc    =    "T"
                     go to det-mzn-max-645.
           if        rf-mmz-cod-arc       not  = w-det-mzn-cod-arc
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-645.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza per il codice ar- *
      *                  * chivio                                      *
      *                  *---------------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-max-650.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     go to det-mzn-max-650.
           if        rf-mmz-tip-ctm       not  = "N"
                     go to det-mzn-max-650.
           if        w-det-mzn-uot-ctm    =    "T"
                     go to det-mzn-max-650.
           if        rf-mmz-cod-ctm       not  = w-det-mzn-cod-ctm
                     go to det-mzn-max-650.
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-max-650.
           if        rf-mmz-tip-arc       not  = w-det-mzn-tip-arc
                     go to det-mzn-max-650.
           if        w-det-mzn-uot-arc    =    "T"
                     go to det-mzn-max-650.
           if        rf-mmz-cod-arc       not  = w-det-mzn-cod-arc
                     go to det-mzn-max-650.
           if        w-det-mzn-uot-dpa    =    "T"
                     go to det-mzn-max-650.
           if        rf-mmz-dpz-arc       not  = w-det-mzn-dpz-arc
                     move  "#"            to   w-det-mzn-max-flg
                     go to det-mzn-max-999.
       det-mzn-max-650.
      *                  *---------------------------------------------*
      *                  * Uscita per test superato                    *
      *                  *---------------------------------------------*
           go to     det-mzn-max-999.
       det-mzn-max-999.
           exit.

      *    *===========================================================*
      *    * Selezione su [mmz] per determinazione saldi da [mmz]      *
      *    *-----------------------------------------------------------*
       det-mzn-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mzn-sel-flg      .
       det-mzn-sel-100.
      *              *-------------------------------------------------*
      *              * Selezione su anno di esercizio                  *
      *              *-------------------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzn-sub-ese
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-150.
      *              *-------------------------------------------------*
      *              * Selezione su codice dipendenza                  *
      *              *-------------------------------------------------*
           if        w-det-mzn-uot-dpz    =    "T"
                     go to det-mzn-sel-200.
           if        rf-mmz-cod-dpz       not  = w-det-mzn-cod-dpz
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-200.
      *              *-------------------------------------------------*
      *              * Selezione su tipo codice di magazzino           *
      *              *-------------------------------------------------*
           if        rf-mmz-tip-mag       not  = w-det-mzn-tip-mag
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-250.
      *              *-------------------------------------------------*
      *              * Selezione su codice numerico di magazzino       *
      *              *-------------------------------------------------*
           if        rf-mmz-num-mag       not  = w-det-mzn-num-mag
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-300.
      *              *-------------------------------------------------*
      *              * Selezione su codice variante                    *
      *              *-------------------------------------------------*
           if        w-det-mzn-uot-var    =    "T"
                     go to det-mzn-sel-350.
           if        rf-mmz-var-mag       not  = w-det-mzn-var-mag
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-350.
      *              *-------------------------------------------------*
      *              * Selezione su tipo conto merce                   *
      *              *-------------------------------------------------*
           if        rf-mmz-tip-ctm       not  = "N"
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-400.
      *              *-------------------------------------------------*
      *              * Selezione su codice conto merce                 *
      *              *-------------------------------------------------*
           if        w-det-mzn-uot-ctm    =    "T"
                     go to det-mzn-sel-450.
           if        rf-mmz-cod-ctm       not  = w-det-mzn-cod-ctm
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-450.
      *              *-------------------------------------------------*
      *              * Selezione su tipo archivio                      *
      *              *-------------------------------------------------*
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-sel-500.
           if        rf-mmz-tip-arc       not  = w-det-mzn-tip-arc
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-500.
      *              *-------------------------------------------------*
      *              * Selezione su codice archivio                    *
      *              *-------------------------------------------------*
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-sel-550.
           if        w-det-mzn-uot-arc    =    "T"
                     go to det-mzn-sel-550.
           if        rf-mmz-cod-arc       not  = w-det-mzn-cod-arc
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-550.
      *              *-------------------------------------------------*
      *              * Selezione su codice dipendenza relativo al co-  *
      *              * dice archivio                                   *
      *              *-------------------------------------------------*
           if        w-det-mzn-tip-arc    =    spaces
                     go to det-mzn-sel-600.
           if        w-det-mzn-uot-arc    =    "T"
                     go to det-mzn-sel-600.
           if        w-det-mzn-uot-dpa    =    "T"
                     go to det-mzn-sel-600.
           if        rf-mmz-dpz-arc       not  = w-det-mzn-dpz-arc
                     move  "#"            to   w-det-mzn-sel-flg
                     go to det-mzn-sel-999.
       det-mzn-sel-600.
      *              *-------------------------------------------------*
      *              * Uscita per selezione superata                   *
      *              *-------------------------------------------------*
           go to     det-mzn-sel-999.
       det-mzn-sel-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldi da [mmz] per tipo conto merce 'T'    *
      *    *                                                           *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *  - w-det-mzt-dat-att = Data attuale                       *
      *    *                                                           *
      *    *  - w-det-mzt-dat-rif = Data di riferimento per determina- *
      *    *                        zione saldi                        *
      *    *                                                           *
      *    *  - w-det-mzt-fin-mes = Flag indicatore di data di riferi- *
      *    *                        mento pari ad un fine mese         *
      *    *                                                           *
      *    *  - w-det-mzt-ism-rif = Indice su mese di esercizio rela-  *
      *    *                        tivo alla data di riferimento per  *
      *    *                        determinazione saldi               *
      *    *                                                           *
      *    *  - w-det-mzt-1o2-ese = Uno o due esercizi da considerare  *
      *    *                                                           *
      *    *  - w-det-mzt-1mo-ese = Primo anno di esercizio da consi-  *
      *    *                        derare                             *
      *    *                                                           *
      *    *  - w-det-mzt-2do-ese = Secondo anno di esercizio da con-  *
      *    *                        siderare                           *
      *    *                                                           *
      *    *  - w-det-mzt-tip-mag = Tipo codice di magazzino           *
      *    *                                                           *
      *    *  - w-det-mzt-num-mag = Codice numerico di magazzino       *
      *    *                                                           *
      *    *  - w-det-mzt-uot-dpz = Una o tutte le dipendenze          *
      *    *                                                           *
      *    *  - w-det-mzt-cod-dpz = Codice dipendenza                  *
      *    *                                                           *
      *    *  - w-det-mzt-uot-var = Una o tutte le varianti            *
      *    *                                                           *
      *    *  - w-det-mzt-var-mag = Codice variante                    *
      *    *                                                           *
      *    *  - w-det-mzt-uot-ctm = Uno o tutti i conti merce          *
      *    *                                                           *
      *    *  - w-det-mzt-cod-ctm = Codice conto merce                 *
      *    *                                                           *
      *    *  - w-det-mzt-tip-arc = Tipo archivio                      *
      *    *                                                           *
      *    *  - w-det-mzt-uot-arc = Uno o tutti i codici archivio      *
      *    *                                                           *
      *    *  - w-det-mzt-cod-arc = Codice archivio                    *
      *    *                                                           *
      *    *  - w-det-mzt-uot-dpa = Una o tutte le dipendenze per il   *
      *    *                        codice archivio                    *
      *    *                                                           *
      *    *  - w-det-mzt-dpz-arc = Codice dipendenza per il codice    *
      *    *                        archivio                           *
      *    *                                                           *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *                                                           *
      *    *  - w-det-mzt-det-dbi = Debito inizio anno                 *
      *    *                                                           *
      *    *  - w-det-mzt-det-tcp = Totale movimenti a nostro credito  *
      *    *                        periodo                            *
      *    *                                                           *
      *    *  - w-det-mzt-det-tdp = Totale movimenti a nostro debito   *
      *    *                        periodo                            *
      *    *                                                           *
      *    *  - w-det-mzt-det-dbf = Debito fine periodo                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-mzt-000.
      *              *-------------------------------------------------*
      *              * Determinazione saldi [mmz] tipo conto merce 'T' *
      *              * 1. esercizio                                    *
      *              *-------------------------------------------------*
           perform   det-mzt-1oe-000      thru det-mzt-1oe-999        .
       det-mzt-050.
      *              *-------------------------------------------------*
      *              * Determinazione saldi [mmz] tipo conto merce 'T' *
      *              * 2. esercizio                                    *
      *              *-------------------------------------------------*
           perform   det-mzt-2oe-000      thru det-mzt-2oe-999        .
       det-mzt-100.
      *              *-------------------------------------------------*
      *              * Determinazione saldi richiesti                  *
      *              *-------------------------------------------------*
       det-mzt-150.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se 1 o 2 esercizi con- *
      *                  * siderati                                    *
      *                  *---------------------------------------------*
           if        w-det-mzt-1o2-ese    =    2
                     go to det-mzt-400.
       det-mzt-200.
      *                  *---------------------------------------------*
      *                  * Se considerato un solo esercizio, ovvero il *
      *                  * secondo                                     *
      *                  *---------------------------------------------*
       det-mzt-250.
      *                      *-----------------------------------------*
      *                      * Debito inizio anno                      *
      *                      *-----------------------------------------*
           move      w-det-mzt-2oe-dbi    to   w-det-mzt-det-dbi      .
      *                      *-----------------------------------------*
      *                      * Totale movimenti a nostro credito del   *
      *                      * periodo                                 *
      *                      *-----------------------------------------*
           move      w-det-mzt-2oe-tcp    to   w-det-mzt-det-tcp      .
      *                      *-----------------------------------------*
      *                      * Totale movimenti a nostro debito del    *
      *                      * periodo                                 *
      *                      *-----------------------------------------*
           move      w-det-mzt-2oe-tdp    to   w-det-mzt-det-tdp      .
      *                      *-----------------------------------------*
      *                      * Debito fine periodo                     *
      *                      *-----------------------------------------*
           move      w-det-mzt-det-dbi    to   w-det-mzt-det-dbf      .
           subtract  w-det-mzt-det-tcp    from w-det-mzt-det-dbf      .
           add       w-det-mzt-det-tdp    to   w-det-mzt-det-dbf      .
       det-mzt-300.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-mzt-999.
       det-mzt-400.
      *                  *---------------------------------------------*
      *                  * Se considerati due esercizi                 *
      *                  *---------------------------------------------*
       det-mzt-450.
      *                      *-----------------------------------------*
      *                      * Debito inizio anno                      *
      *                      *-----------------------------------------*
           move      w-det-mzt-1oe-dbi    to   w-det-mzt-det-dbi      .
           subtract  w-det-mzt-1oe-tca    from w-det-mzt-det-dbi      .
           add       w-det-mzt-1oe-tda    to   w-det-mzt-det-dbi      .
      *                      *-----------------------------------------*
      *                      * Totale movimenti a nostro credito del   *
      *                      * periodo                                 *
      *                      *-----------------------------------------*
           move      w-det-mzt-2oe-tcp    to   w-det-mzt-det-tcp      .
      *                      *-----------------------------------------*
      *                      * Totale movimenti a nostro debito del    *
      *                      * periodo                                 *
      *                      *-----------------------------------------*
           move      w-det-mzt-2oe-tdp    to   w-det-mzt-det-tdp      .
      *                      *-----------------------------------------*
      *                      * Debito fine periodo                     *
      *                      *-----------------------------------------*
           move      w-det-mzt-det-dbi    to   w-det-mzt-det-dbf      .
           subtract  w-det-mzt-det-tcp    from w-det-mzt-det-dbf      .
           add       w-det-mzt-det-tdp    to   w-det-mzt-det-dbf      .
       det-mzt-500.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-mzt-999.
       det-mzt-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldi [mmz] tipo conto merce 'T' relative  *
      *    * al 1. esercizio                                           *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-det-mzt-1oe-dbi = Debito inizio anno 1. esercizio    *
      *    *                                                           *
      *    *  - w-det-mzt-1oe-tca = Totale movimenti a nostro credito  *
      *    *                        anno 1. esercizio                  *
      *    *                                                           *
      *    *  - w-det-mzt-1oe-tda = Totale movimenti a nostro debito   *
      *    *                        anno 1. esercizio                  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-mzt-1oe-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Debito inizio anno 1. esercizio             *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzt-1oe-dbi      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti nostro credito anno 1. e-  *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzt-1oe-tca      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti nostro debito anno 1. e-   *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzt-1oe-tda      .
       det-mzt-1oe-100.
      *              *-------------------------------------------------*
      *              * Se non sono da considerare due esercizi : usci- *
      *              * ta                                              *
      *              *-------------------------------------------------*
           if        w-det-mzt-1o2-ese    not  = 2
                     go to det-mzt-1oe-999.
       det-mzt-1oe-200.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per subroutines di scan- *
      *              * sione [mmz] per 1. esercizio                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anno di esercizio da considerare : il 1. e- *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      w-det-mzt-1mo-ese    to   w-det-mzt-sub-ese      .
       det-mzt-1oe-300.
      *              *-------------------------------------------------*
      *              * Start su [mmz] per 1. esercizio                 *
      *              *-------------------------------------------------*
           perform   det-mzt-str-000      thru det-mzt-str-999        .
      *              *-------------------------------------------------*
      *              * Se start errata : uscita                        *
      *              *-------------------------------------------------*
           if        w-det-mzt-str-flg    not  = spaces
                     go to det-mzt-1oe-999.
       det-mzt-1oe-400.
      *              *-------------------------------------------------*
      *              * Read next su [mmz] per 1. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mzt-nxt-000      thru det-mzt-nxt-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita                           *
      *              *-------------------------------------------------*
           if        w-det-mzt-nxt-flg    not  = spaces
                     go to det-mzt-1oe-999.
       det-mzt-1oe-500.
      *              *-------------------------------------------------*
      *              * Test max su [mmz] per 1. esercizio              *
      *              *-------------------------------------------------*
           perform   det-mzt-max-000      thru det-mzt-max-999        .
      *              *-------------------------------------------------*
      *              * Se test max non superato : uscita               *
      *              *-------------------------------------------------*
           if        w-det-mzt-max-flg    not  = spaces
                     go to det-mzt-1oe-999.
       det-mzt-1oe-600.
      *              *-------------------------------------------------*
      *              * Selezioni su [mmz] per 1. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mzt-sel-000      thru det-mzt-sel-999        .
      *              *-------------------------------------------------*
      *              * Se selezioni non superate : riciclo su Next     *
      *              *-------------------------------------------------*
           if        w-det-mzt-sel-flg    not  = spaces
                     go to det-mzt-1oe-400.
       det-mzt-1oe-700.
      *              *-------------------------------------------------*
      *              * Elaborazioni sul record di [mmz] in esame       *
      *              *-------------------------------------------------*
       det-mzt-1oe-710.
      *                  *---------------------------------------------*
      *                  * Sommatoria della quantita' a nostro debito  *
      *                  * ad inizio esercizio                         *
      *                  *---------------------------------------------*
           add       rf-mmz-qta-ini       to   w-det-mzt-1oe-dbi      .
       det-mzt-1oe-720.
      *                  *---------------------------------------------*
      *                  * Sommatoria movimenti a nostro credito e a   *
      *                  * nostro debito per l'intero esercizio        *
      *                  *---------------------------------------------*
       det-mzt-1oe-730.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice 1..12 su mesi   *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-mzt-inx-mes      .
       det-mzt-1oe-740.
      *                      *-----------------------------------------*
      *                      * Incremento indice su mesi               *
      *                      *-----------------------------------------*
           add       1                    to   w-det-mzt-inx-mes      .
      *                      *-----------------------------------------*
      *                      * Se oltre ultimo mese : fine ciclo       *
      *                      *-----------------------------------------*
           if        w-det-mzt-inx-mes    >    12
                     go to det-mzt-1oe-750.
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro credito   *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-cre
                    (w-det-mzt-inx-mes)   to   w-det-mzt-1oe-tca      .
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro debito    *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-deb
                    (w-det-mzt-inx-mes)   to   w-det-mzt-1oe-tda      .
      *                      *-----------------------------------------*
      *                      * Riciclo su mese successivo              *
      *                      *-----------------------------------------*
           go to     det-mzt-1oe-740.
       det-mzt-1oe-750.
      *              *-------------------------------------------------*
      *              * Riciclo su Next                                 *
      *              *-------------------------------------------------*
           go to     det-mzt-1oe-400.
       det-mzt-1oe-999.
           exit.

      *    *===========================================================*
      *    * Determinazione saldi [mmz] tipo conto merce 'T' relative  *
      *    * al 2. esercizio                                           *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-det-mzt-2oe-dbi = Debito inizio anno 2. esercizio    *
      *    *                                                           *
      *    *  - w-det-mzt-2oe-tcp = Totale movimenti a nostro credito  *
      *    *                        periodo 2. esercizio               *
      *    *                                                           *
      *    *  - w-det-mzt-2oe-tdp = Totale movimenti a nostro debito   *
      *    *                        periodo 2. esercizio               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-mzt-2oe-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Debito inizio anno 2. esercizio             *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzt-2oe-dbi      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti nostro credito periodo 2.  *
      *                  * esercizio                                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzt-2oe-tcp      .
      *                  *---------------------------------------------*
      *                  * Totale movimenti nostro debito periodo 2.   *
      *                  * esercizio                                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mzt-2oe-tdp      .
       det-mzt-2oe-050.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per subroutines di scan- *
      *              * sione [mmz] per 2. esercizio                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anno di esercizio da considerare : il 2. e- *
      *                  * sercizio                                    *
      *                  *---------------------------------------------*
           move      w-det-mzt-2do-ese    to   w-det-mzt-sub-ese      .
       det-mzt-2oe-100.
      *              *-------------------------------------------------*
      *              * Start su [mmz] per 2. esercizio                 *
      *              *-------------------------------------------------*
           perform   det-mzt-str-000      thru det-mzt-str-999        .
      *              *-------------------------------------------------*
      *              * Se start errata : a correzioni finali           *
      *              *-------------------------------------------------*
           if        w-det-mzt-str-flg    not  = spaces
                     go to det-mzt-2oe-450.
       det-mzt-2oe-150.
      *              *-------------------------------------------------*
      *              * Read next su [mmz] per 2. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mzt-nxt-000      thru det-mzt-nxt-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : a correzioni finali              *
      *              *-------------------------------------------------*
           if        w-det-mzt-nxt-flg    not  = spaces
                     go to det-mzt-2oe-450.
       det-mzt-2oe-200.
      *              *-------------------------------------------------*
      *              * Test max su [mmz] per 2. esercizio              *
      *              *-------------------------------------------------*
           perform   det-mzt-max-000      thru det-mzt-max-999        .
      *              *-------------------------------------------------*
      *              * Se test max non superato : a correzioni finali  *
      *              *-------------------------------------------------*
           if        w-det-mzt-max-flg    not  = spaces
                     go to det-mzt-2oe-450.
       det-mzt-2oe-250.
      *              *-------------------------------------------------*
      *              * Selezioni su [mmz] per 2. esercizio             *
      *              *-------------------------------------------------*
           perform   det-mzt-sel-000      thru det-mzt-sel-999        .
      *              *-------------------------------------------------*
      *              * Se selezioni non superate : riciclo su Next     *
      *              *-------------------------------------------------*
           if        w-det-mzt-sel-flg    not  = spaces
                     go to det-mzt-2oe-150.
       det-mzt-2oe-300.
      *              *-------------------------------------------------*
      *              * Elaborazioni sul record di [mmz] in esame       *
      *              *-------------------------------------------------*
       det-mzt-2oe-310.
      *                  *---------------------------------------------*
      *                  * Sommatoria della quantita' a nostro debito  *
      *                  * ad inizio esercizio                         *
      *                  *---------------------------------------------*
           add       rf-mmz-qta-ini       to   w-det-mzt-2oe-dbi      .
       det-mzt-2oe-320.
      *                  *---------------------------------------------*
      *                  * Sommatoria movimenti a nostro credito ed a  *
      *                  * nostro debito per il periodo antecedente il *
      *                  * mese di esercizio relativo alla data di ri- *
      *                  * ferimento per determinazione saldi          *
      *                  *---------------------------------------------*
       det-mzt-2oe-325.
      *                      *-----------------------------------------*
      *                      * Se l'indice sul mese e' a zero signifi- *
      *                      * ca che interessa solamente il saldo di  *
      *                      * inizio esercizio, quindi si escludono   *
      *                      * tutte le altre sommatorie               *
      *                      *-----------------------------------------*
           if        w-det-mzt-ism-rif    =    zero
                     go to det-mzt-2oe-400.
       det-mzt-2oe-330.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice su mesi pari    *
      *                      * all'indice su mese di esercizio rela-   *
      *                      * tivo alla data di riferimento per la    *
      *                      * determinazione saldi                    *
      *                      *-----------------------------------------*
           move      w-det-mzt-ism-rif    to   w-det-mzt-inx-mes      .
       det-mzt-2oe-340.
      *                      *-----------------------------------------*
      *                      * Decremento indice su mesi               *
      *                      *-----------------------------------------*
           subtract  1                    from w-det-mzt-inx-mes      .
      *                      *-----------------------------------------*
      *                      * Se giunti a zero : fine ciclo           *
      *                      *-----------------------------------------*
           if        w-det-mzt-inx-mes    =    zero
                     go to det-mzt-2oe-350.
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro credito   *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-cre
                    (w-det-mzt-inx-mes)   to   w-det-mzt-2oe-tcp      .
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro debito    *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-deb
                    (w-det-mzt-inx-mes)   to   w-det-mzt-2oe-tdp      .
      *                      *-----------------------------------------*
      *                      * Riciclo su mese precedente              *
      *                      *-----------------------------------------*
           go to     det-mzt-2oe-340.
       det-mzt-2oe-350.
      *                  *---------------------------------------------*
      *                  * Se la data di riferimento corrisponde alla  *
      *                  * data attuale oppure se la data di riferi-   *
      *                  * mento corrisponde ad un fine mese, si ac-   *
      *                  * cumulano anche i movimenti a credito ed a   *
      *                  * debito relativi al mese di esercizio rela-  *
      *                  * tivo alla data di riferimento per determi-  *
      *                  * nazione saldi                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-det-mzt-dat-rif    not  = w-det-mzt-dat-att and
                     w-det-mzt-fin-mes    not  = "F"
                     go to det-mzt-2oe-400.
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro credito   *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-cre
                    (w-det-mzt-ism-rif)   to   w-det-mzt-2oe-tcp      .
      *                      *-----------------------------------------*
      *                      * Sommatoria movimenti a nostro debito    *
      *                      *-----------------------------------------*
           add       rf-mmz-prg-deb
                    (w-det-mzt-ism-rif)   to   w-det-mzt-2oe-tdp      .
       det-mzt-2oe-400.
      *              *-------------------------------------------------*
      *              * Riciclo su Next                                 *
      *              *-------------------------------------------------*
           go to     det-mzt-2oe-150.
       det-mzt-2oe-450.
      *              *-------------------------------------------------*
      *              * Correzioni finali                               *
      *              *-------------------------------------------------*
       det-mzt-2oe-500.
      *                  *---------------------------------------------*
      *                  * Se l'indice sul mese e' a zero significa    *
      *                  * che interessa solamente il saldo di inizio  *
      *                  * esercizio, quindi si esce immediatamente    *
      *                  * senza effettuare le correzioni finali, in   *
      *                  * quanto non necessarie                       *
      *                  *---------------------------------------------*
           if        w-det-mzt-ism-rif    =    zero
                     go to det-mzt-2oe-999.
      *                  *---------------------------------------------*
      *                  * Se la data di riferimento corrisponde alla  *
      *                  * data attuale oppure se la data di riferi-   *
      *                  * mento corrisponde ad un fine mese, si esce  *
      *                  * immediatamente senza effettuare le corre-   *
      *                  * zioni finali, in quanto non necessarie      *
      *                  *---------------------------------------------*
           if        w-det-mzt-dat-rif    =    w-det-mzt-dat-att or
                     w-det-mzt-fin-mes    =    "F"
                     go to det-mzt-2oe-999.
      *                  *---------------------------------------------*
      *                  * Preparazione della data relativa al primo   *
      *                  * giorno del mese della della data di rife-   *
      *                  * rimento                                     *
      *                  *---------------------------------------------*
           move      w-det-mzt-dat-rif    to   s-dat                  .
           move      01                   to   s-gio                  .
           move      s-dat                to   w-det-mzt-g1m-ddr      .
       det-mzt-2oe-550.
      *                  *---------------------------------------------*
      *                  * Start su [mmr]                              *
      *                  *---------------------------------------------*
       det-mzt-2oe-555.
      *                      *-----------------------------------------*
      *                      * Deviazione se una o tutte le dipendenze *
      *                      *-----------------------------------------*
           if        w-det-mzt-uot-dpz    =    "T"
                     go to det-mzt-2oe-565.
       det-mzt-2oe-560.
      *                      *-----------------------------------------*
      *                      * Se una sola dipendenza                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPZMAG    "         to   f-key                  .
           move      w-det-mzt-cod-dpz    to   rf-mmr-cod-dpz         .
           move      w-det-mzt-tip-mag    to   rf-mmr-tip-mag         .
           move      w-det-mzt-num-mag    to   rf-mmr-num-mag         .
           move      w-det-mzt-g1m-ddr    to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                          *-------------------------------------*
      *                          * A controllo esito Start             *
      *                          *-------------------------------------*
           go to     det-mzt-2oe-570.
       det-mzt-2oe-565.
      *                      *-----------------------------------------*
      *                      * Se tutte le dipendenze                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      w-det-mzt-tip-mag    to   rf-mmr-tip-mag         .
           move      w-det-mzt-num-mag    to   rf-mmr-num-mag         .
           move      w-det-mzt-g1m-ddr    to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                          *-------------------------------------*
      *                          * A controllo esito Start             *
      *                          *-------------------------------------*
           go to     det-mzt-2oe-570.
       det-mzt-2oe-570.
      *                      *-----------------------------------------*
      *                      * Se Start errata : uscita                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-mzt-2oe-999.
       det-mzt-2oe-600.
      *                  *---------------------------------------------*
      *                  * Read Next su [mmr]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-mzt-2oe-999.
       det-mzt-2oe-650.
      *                  *---------------------------------------------*
      *                  * Test Max su [mmr], se non superato : uscita *
      *                  *---------------------------------------------*
       det-mzt-2oe-655.
      *                      *-----------------------------------------*
      *                      * Deviazione se una o tutte le dipendenze *
      *                      *-----------------------------------------*
           if        w-det-mzt-uot-dpz    =    "T"
                     go to det-mzt-2oe-665.
       det-mzt-2oe-660.
      *                      *-----------------------------------------*
      *                      * Se una sola dipendenza                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Max su codice dipendenza            *
      *                          *-------------------------------------*
           if        rf-mmr-cod-dpz       not  = w-det-mzt-cod-dpz
                     go to det-mzt-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su tipo codice magazzino        *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mag       not  = w-det-mzt-tip-mag
                     go to det-mzt-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su codice numerico magazzino    *
      *                          *-------------------------------------*
           if        rf-mmr-num-mag       not  = w-det-mzt-num-mag
                     go to det-mzt-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su data registrazione           *
      *                          *-------------------------------------*
           if        rf-mmr-dat-reg       >    w-det-mzt-dat-rif
                     go to det-mzt-2oe-999.
      *                          *-------------------------------------*
      *                          * Test max superato                   *
      *                          *-------------------------------------*
           go to     det-mzt-2oe-700.
       det-mzt-2oe-665.
      *                      *-----------------------------------------*
      *                      * Se tutte le dipendenze                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Max su tipo codice magazzino        *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mag       not  = w-det-mzt-tip-mag
                     go to det-mzt-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su codice numerico magazzino    *
      *                          *-------------------------------------*
           if        rf-mmr-num-mag       not  = w-det-mzt-num-mag
                     go to det-mzt-2oe-999.
      *                          *-------------------------------------*
      *                          * Max su data registrazione           *
      *                          *-------------------------------------*
           if        rf-mmr-dat-reg       >    w-det-mzt-dat-rif
                     go to det-mzt-2oe-999.
      *                          *-------------------------------------*
      *                          * Test max superato                   *
      *                          *-------------------------------------*
           go to     det-mzt-2oe-700.
       det-mzt-2oe-700.
      *                  *---------------------------------------------*
      *                  * Selezione su [mmr], se non superata : rici- *
      *                  * clo a lettura                               *
      *                  *---------------------------------------------*
       det-mzt-2oe-705.
      *                      *-----------------------------------------*
      *                      * Sel su data registrazione               *
      *                      *                                         *
      *                      * Se non compresa tra il primo giorno del *
      *                      * mese relativo alla data di riferimento, *
      *                      * e la data di riferimento stessa : sele- *
      *                      * zione non superata                      *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-dat-reg       <    w-det-mzt-g1m-ddr or
                     rf-mmr-dat-reg       >    w-det-mzt-dat-rif
                     go to det-mzt-2oe-600.
       det-mzt-2oe-710.
      *                      *-----------------------------------------*
      *                      * Sel su codice dipendenza                *
      *                      *                                         *
      *                      * Se una sola dipendenza, ma la dipenden- *
      *                      * za del movimento letto non corrisponde  *
      *                      * a quella richiesta : selezione non su-  *
      *                      * perata                                  *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzt-uot-dpz    not  = "T"
                     if    rf-mmr-cod-dpz not  = w-det-mzt-cod-dpz
                           go to  det-mzt-2oe-600.
       det-mzt-2oe-715.
      *                      *-----------------------------------------*
      *                      * Sel su tipo di trattamento per il valo- *
      *                      * re per la causale                       *
      *                      *                                         *
      *                      * Se movimento di solo valore : selezione *
      *                      * non superata                            *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-val       =    "C" or
                     rf-mmr-trt-val       =    "X" or
                     rf-mmr-trt-val       =    "R" or
                     rf-mmr-trt-val       =    "Y"
                     go to det-mzt-2oe-600.
       det-mzt-2oe-720.
      *                      *-----------------------------------------*
      *                      * Sel su tipo movimentazione di magazzino *
      *                      *                                         *
      *                      * Se movimento di carico o scarico : se-  *
      *                      * lezione superata                        *
      *                      *                                         *
      *                      * Se movimentazione interna : selezione   *
      *                      * non superata                            *
      *                      *                                         *
      *                      * Se tipo movimento non riconosciuto :    *
      *                      * selezione non superata                  *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mdm       =    01 or
                     rf-mmr-tip-mdm       =    02
                     go to det-mzt-2oe-725
           else if   rf-mmr-tip-mdm       =    03
                     go to det-mzt-2oe-600
           else      go to det-mzt-2oe-600.
       det-mzt-2oe-725.
      *                      *-----------------------------------------*
      *                      * Sel su trattamento conto merce e codice *
      *                      * conto merce                             *
      *                      *                                         *
      *                      * Se il tipo trattamento merce in conto   *
      *                      * indica che il movimento puo' riferir-   *
      *                      * si, anche se non necessariamente, ad    *
      *                      * un precedente movimento di merce in     *
      *                      * conto                                   *
      *                      *                                         *
      *                      *    Se non e' stato citato alcun conto   *
      *                      *    merce : selezione non superata       *
      *                      *                                         *
      *                      *    Altrimenti : selezione superata      *
      *                      *                                         *
      *                      * Se il movimento non puo' riferirsi ad   *
      *                      * un altro documento : selezione non su-  *
      *                      * perata                                  *
      *                      *                                         *
      *                      * Se il movimento stesso e' una movimen-  *
      *                      * tazione di merce in conto : selezione   *
      *                      * superata                                *
      *                      *                                         *
      *                      * Altrimenti : selezione non superata     *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-mic       =    01
                     if    rf-mmr-cod-ctm =    spaces
                           go to det-mzt-2oe-600
                     else  go to det-mzt-2oe-730
           else if   rf-mmr-trt-mic       =    02
                     go to det-mzt-2oe-600
           else if   rf-mmr-trt-mic       =    03
                     go to det-mzt-2oe-730
           else      go to det-mzt-2oe-600.
       det-mzt-2oe-730.
      *                      *-----------------------------------------*
      *                      * Sel su tipo conto merce                 *
      *                      *                                         *
      *                      * Se diverso da 'T' : selezione non su-   *
      *                      * perata                                  *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mic       not  = "T"
                     go to det-mzt-2oe-600.
       det-mzt-2oe-735.
      *                      *-----------------------------------------*
      *                      * Sel su codice conto merce               *
      *                      *                                         *
      *                      * Se a spaces : selezione non superata    *
      *                      *                                         *
      *                      * Se un solo conto merce, ma il conto     *
      *                      * merce del movimento letto non corri-    *
      *                      * sponde a quello richiesto : selezio-    *
      *                      * ne non superata                         *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-cod-ctm       =    spaces
                     go to det-mzt-2oe-600.
           if        w-det-mzt-uot-ctm    not  = "T"
                     if    rf-mmr-cod-ctm not  = w-det-mzt-cod-ctm
                           go to  det-mzt-2oe-600.
       det-mzt-2oe-740.
      *                      *-----------------------------------------*
      *                      * Sel su tipo archivio                    *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i tipi ar- *
      *                      * chivio : selezione superata             *
      *                      *                                         *
      *                      * Se un solo tipo archivio, ma il tipo    *
      *                      * archivio del movimento letto non cor-   *
      *                      * risponde a quello richiesto : sele-     *
      *                      * zione non superata                      *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-2oe-745.
           if        rf-mmr-tip-arc       not  = w-det-mzt-tip-arc
                     go to det-mzt-2oe-600.
       det-mzt-2oe-745.
      *                      *-----------------------------------------*
      *                      * Sel su codice archivio                  *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i tipi ar- *
      *                      * chivio : selezione superata             *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i codici   *
      *                      * archivio : selezione superata           *
      *                      *                                         *
      *                      * Se un solo codice archivio, ma il co-   *
      *                      * dice archivio del movimento letto non   *
      *                      * corrisponde a quello richiesto : sele-  *
      *                      * zione non superata                      *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-2oe-750.
           if        w-det-mzt-uot-arc    =    "T"
                     go to det-mzt-2oe-750.
           if        rf-mmr-cod-arc       not  = w-det-mzt-cod-arc
                     go to det-mzt-2oe-600.
       det-mzt-2oe-750.
      *                      *-----------------------------------------*
      *                      * Sel su codice dipendenza per il codice  *
      *                      * archivio                                *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i tipi ar- *
      *                      * chivio : selezione superata             *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i codici   *
      *                      * archivio : selezione superata           *
      *                      *                                         *
      *                      * Se sono da selezionare tutti i codici   *
      *                      * dipendenza per il codice archivio : se- *
      *                      * lezione superata                        *
      *                      *                                         *
      *                      * Se un solo codice dipendenza per il co- *
      *                      * dice archivio, ma il codice dipendenza  *
      *                      * per il codice archivio del movimento    *
      *                      * letto non corrisponde a quello richie-  *
      *                      * sto : selezione non superata            *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-2oe-755.
           if        w-det-mzt-uot-arc    =    "T"
                     go to det-mzt-2oe-755.
           if        w-det-mzt-uot-dpa    =    "T"
                     go to det-mzt-2oe-755.
           if        rf-mmr-dpz-arc       not  = w-det-mzt-dpz-arc
                     go to det-mzt-2oe-600.
       det-mzt-2oe-755.
      *                      *-----------------------------------------*
      *                      * Sel su tipo codice magazzino            *
      *                      *                                         *
      *                      * Se non corrisponde a quello richiesto : *
      *                      * selezione non superata                  *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mag       not  = w-det-mzt-tip-mag
                     go to det-mzt-2oe-600.
       det-mzt-2oe-760.
      *                      *-----------------------------------------*
      *                      * Sel su codice numerico magazzino        *
      *                      *                                         *
      *                      * Se non corrisponde a quello richiesto : *
      *                      * selezione non superata                  *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        rf-mmr-num-mag       not  = w-det-mzt-num-mag
                     go to det-mzt-2oe-600.
       det-mzt-2oe-765.
      *                      *-----------------------------------------*
      *                      * Sel su variante al codice magazzino     *
      *                      *                                         *
      *                      * Se tutte le varianti : selezione supe-  *
      *                      * rata                                    *
      *                      *                                         *
      *                      * Se una sola variante, ma il codice va-  *
      *                      * riante del movimento letto non corri-   *
      *                      * sponde a quello richiesto : selezione   *
      *                      * non superata                            *
      *                      *                                         *
      *                      * Altrimenti : selezione superata         *
      *                      *-----------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-2oe-770.
           if        rf-mmr-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-2oe-600.
       det-mzt-2oe-770.
      *                      *-----------------------------------------*
      *                      * Selezione su [mmr] superata             *
      *                      *-----------------------------------------*
           go to     det-mzt-2oe-800.
       det-mzt-2oe-800.
      *                  *---------------------------------------------*
      *                  * Elaborazioni sul record di [mmr] in esame : *
      *                  * simulazione di aggiornamento                *
      *                  *---------------------------------------------*
       det-mzt-2oe-825.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se si deve aggior- *
      *                      * nare il credito o il debito.            *
      *                      *                                         *
      *                      * Se il tipo trattamento merce in conto   *
      *                      * indica che il movimento puo' riferir-   *
      *                      * si, anche se non necessariamente, ad    *
      *                      * un precedente movimento di merce in     *
      *                      * conto :                                 *
      *                      *                                         *
      *                      *    Se movimento di carico si aggiorna   *
      *                      *    il progressivo credito               *
      *                      *                                         *
      *                      *    Se movimento di scarico si aggiorna  *
      *                      *    il progressivo debito                *
      *                      *                                         *
      *                      *    Altrimenti nessun aggiornamento      *
      *                      *                                         *
      *                      * Se il movimento stesso e' una movimen-  *
      *                      * tazione di merce in conto :             *
      *                      *                                         *
      *                      *    Se movimento di carico si aggiorna   *
      *                      *    il progressivo debito                *
      *                      *                                         *
      *                      *    Se movimento di scarico si aggiorna  *
      *                      *    il progressivo credito               *
      *                      *                                         *
      *                      *    Altrimenti nessun aggiornamento      *
      *                      *                                         *
      *                      * Altrimenti nessun aggiornamento         *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-mic       =    01
                     if      rf-mmr-tip-mdm
                                          =    01
                             go to det-mzt-2oe-850
                     else if rf-mmr-tip-mdm
                                          =    02
                             go to det-mzt-2oe-875
                     else    go to det-mzt-2oe-900
           else if   rf-mmr-trt-mic       =    03
                     if      rf-mmr-tip-mdm
                                          =    01
                             go to det-mzt-2oe-875
                     else if rf-mmr-tip-mdm
                                          =    02
                             go to det-mzt-2oe-850
                     else    go to det-mzt-2oe-900
           else      go to   det-mzt-2oe-900.
       det-mzt-2oe-850.
      *                      *-----------------------------------------*
      *                      * Aggiornamento progressivo credito       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Sommatoria progressivo credito      *
      *                          *-------------------------------------*
           add       rf-mmr-qta-mov       to   w-det-mzt-2oe-tcp      .
      *                          *-------------------------------------*
      *                          * A fine elaborazione su record [mmz] *
      *                          * in esame                            *
      *                          *-------------------------------------*
           go to     det-mzt-2oe-900.
       det-mzt-2oe-875.
      *                      *-----------------------------------------*
      *                      * Aggiornamento progressivo debito        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Sommatoria progressivo debito       *
      *                          *-------------------------------------*
           add       rf-mmr-qta-mov       to   w-det-mzt-2oe-tdp      .
      *                          *-------------------------------------*
      *                          * A fine elaborazione su record [mmz] *
      *                          * in esame                            *
      *                          *-------------------------------------*
           go to     det-mzt-2oe-900.
       det-mzt-2oe-900.
      *                      *-----------------------------------------*
      *                      * Fine elaborazione su record [mmz] in e- *
      *                      * same                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Riciclo su Read Next su [mmr]       *
      *                          *-------------------------------------*
           go to     det-mzt-2oe-600.
       det-mzt-2oe-999.
           exit.

      *    *===========================================================*
      *    * Start su [mmz] per determinazione saldi da [mmz]          *
      *    *-----------------------------------------------------------*
       det-mzt-str-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mzt-str-flg      .
       det-mzt-str-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se una o tutte le dipen-   *
      *              * denze                                           *
      *              *-------------------------------------------------*
           if        w-det-mzt-uot-dpz    =    "T"
                     go to det-mzt-str-500.
       det-mzt-str-200.
      *              *-------------------------------------------------*
      *              * Se una sola dipendenza                          *
      *              *-------------------------------------------------*
       det-mzt-str-250.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se un solo tipo archi- *
      *                  * vio o tutti i tipi archivio                 *
      *                  *---------------------------------------------*
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-str-450.
       det-mzt-str-300.
      *                  *---------------------------------------------*
      *                  * Se un solo tipo archivio                    *
      *                  *---------------------------------------------*
       det-mzt-str-325.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se uno o tutti i   *
      *                      * codici archivio                         *
      *                      *-----------------------------------------*
           if        w-det-mzt-uot-arc    =    "T"
                     go to det-mzt-str-400.
       det-mzt-str-350.
      *                      *-----------------------------------------*
      *                      * Se un solo codice archivio              *
      *                      *-----------------------------------------*
       det-mzt-str-375.
      *                          *-------------------------------------*
      *                          * Start per chiave 'ARCMAG'           *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ARCMAG    "         to   f-key                  .
           move      w-det-mzt-sub-ese    to   rf-mmz-ann-ese         .
           move      w-det-mzt-cod-dpz    to   rf-mmz-cod-dpz         .
           move      w-det-mzt-tip-arc    to   rf-mmz-tip-arc         .
           move      w-det-mzt-cod-arc    to   rf-mmz-cod-arc         .
           if        w-det-mzt-uot-dpa    =    "T"
                     move  spaces         to   rf-mmz-dpz-arc
           else      move  w-det-mzt-dpz-arc
                                          to   rf-mmz-dpz-arc         .
           move      w-det-mzt-tip-mag    to   rf-mmz-tip-mag         .
           move      w-det-mzt-num-mag    to   rf-mmz-num-mag         .
           if        w-det-mzt-uot-var    =    "T"
                     move  spaces         to   rf-mmz-var-mag
           else      move  w-det-mzt-var-mag
                                          to   rf-mmz-var-mag         .
           move      "T"                  to   rf-mmz-tip-ctm         .
           if        w-det-mzt-uot-ctm    =    "T"
                     move  spaces         to   rf-mmz-cod-ctm
           else      move  w-det-mzt-cod-ctm
                                          to   rf-mmz-cod-ctm         .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzt-str-380.
      *                          *-------------------------------------*
      *                          * Se start errata : uscita con flag   *
      *                          * di errore                           *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzt-str-flg
                     go to det-mzt-str-999.
       det-mzt-str-385.
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-mzt-str-999.
       det-mzt-str-400.
      *                      *-----------------------------------------*
      *                      * Se tutti i codici archivio              *
      *                      *-----------------------------------------*
       det-mzt-str-405.
      *                          *-------------------------------------*
      *                          * Start per chiave 'MAGARC'           *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGARC    "         to   f-key                  .
           move      w-det-mzt-sub-ese    to   rf-mmz-ann-ese         .
           move      w-det-mzt-cod-dpz    to   rf-mmz-cod-dpz         .
           move      w-det-mzt-tip-mag    to   rf-mmz-tip-mag         .
           move      w-det-mzt-num-mag    to   rf-mmz-num-mag         .
           if        w-det-mzt-uot-var    =    "T"
                     move  spaces         to   rf-mmz-var-mag
           else      move  w-det-mzt-var-mag
                                          to   rf-mmz-var-mag         .
           move      "T"                  to   rf-mmz-tip-ctm         .
           if        w-det-mzt-uot-ctm    =    "T"
                     move  spaces         to   rf-mmz-cod-ctm
           else      move  w-det-mzt-cod-ctm
                                          to   rf-mmz-cod-ctm         .
           move      w-det-mzt-tip-arc    to   rf-mmz-tip-arc         .
           move      zero                 to   rf-mmz-cod-arc         .
           move      spaces               to   rf-mmz-dpz-arc         .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzt-str-410.
      *                          *-------------------------------------*
      *                          * Se start errata : uscita con flag   *
      *                          * di errore                           *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzt-str-flg
                     go to det-mzt-str-999.
       det-mzt-str-415.
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-mzt-str-999.
       det-mzt-str-450.
      *                  *---------------------------------------------*
      *                  * Se tutti i tipi archivio                    *
      *                  *---------------------------------------------*
       det-mzt-str-455.
      *                      *-----------------------------------------*
      *                      * Start per chiave 'MAGARC'               *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGARC    "         to   f-key                  .
           move      w-det-mzt-sub-ese    to   rf-mmz-ann-ese         .
           move      w-det-mzt-cod-dpz    to   rf-mmz-cod-dpz         .
           move      w-det-mzt-tip-mag    to   rf-mmz-tip-mag         .
           move      w-det-mzt-num-mag    to   rf-mmz-num-mag         .
           if        w-det-mzt-uot-var    =    "T"
                     move  spaces         to   rf-mmz-var-mag
           else      move  w-det-mzt-var-mag
                                          to   rf-mmz-var-mag         .
           move      "T"                  to   rf-mmz-tip-ctm         .
           if        w-det-mzt-uot-ctm    =    "T"
                     move  spaces         to   rf-mmz-cod-ctm
           else      move  w-det-mzt-cod-ctm
                                          to   rf-mmz-cod-ctm         .
           move      spaces               to   rf-mmz-tip-arc         .
           move      zero                 to   rf-mmz-cod-arc         .
           move      spaces               to   rf-mmz-dpz-arc         .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzt-str-460.
      *                      *-----------------------------------------*
      *                      * Se start errata : uscita con flag di    *
      *                      * errore                                  *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzt-str-flg
                     go to det-mzt-str-999.
       det-mzt-str-465.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-mzt-str-999.
       det-mzt-str-500.
      *              *-------------------------------------------------*
      *              * Se tutte le dipendenze                          *
      *              *-------------------------------------------------*
       det-mzt-str-505.
      *                  *---------------------------------------------*
      *                  * Start per chiave 'MGCMAR'                   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MGCMAR    "         to   f-key                  .
           move      w-det-mzt-sub-ese    to   rf-mmz-ann-ese         .
           move      w-det-mzt-tip-mag    to   rf-mmz-tip-mag         .
           move      w-det-mzt-num-mag    to   rf-mmz-num-mag         .
           if        w-det-mzt-uot-var    =    "T"
                     move  spaces         to   rf-mmz-var-mag
           else      move  w-det-mzt-var-mag
                                          to   rf-mmz-var-mag         .
           move      "T"                  to   rf-mmz-tip-ctm         .
           if        w-det-mzt-uot-ctm    =    "T"
                     move  spaces         to   rf-mmz-cod-ctm
           else      move  w-det-mzt-cod-ctm
                                          to   rf-mmz-cod-ctm         .
           if        w-det-mzt-tip-arc    =    spaces
                     move  spaces         to   rf-mmz-tip-arc
                     move  zero           to   rf-mmz-cod-arc
                     move  spaces         to   rf-mmz-dpz-arc
           else if   w-det-mzt-uot-arc    =    "T"
                     move  w-det-mzt-tip-arc
                                          to   rf-mmz-tip-arc
                     move  zero           to   rf-mmz-cod-arc
                     move  spaces         to   rf-mmz-dpz-arc
           else      move  w-det-mzt-tip-arc
                                          to   rf-mmz-tip-arc
                     move  w-det-mzt-cod-arc
                                          to   rf-mmz-cod-arc
                     if    w-det-mzt-uot-dpa
                                          =    "T"
                           move  spaces   to   rf-mmz-dpz-arc
                     else  move  w-det-mzt-dpz-arc
                                          to   rf-mmz-dpz-arc         .
           move      zero                 to   rf-mmz-cod-dpz         .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzt-str-415.
      *                  *---------------------------------------------*
      *                  * Se start errata : uscita con flag di errore *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzt-str-flg
                     go to det-mzt-str-999.
       det-mzt-str-420.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-mzt-str-999.
       det-mzt-str-999.
           exit.

      *    *===========================================================*
      *    * Read next su [mmz] per determinazione saldi da [mmz]      *
      *    *-----------------------------------------------------------*
       det-mzt-nxt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mzt-nxt-flg      .
       det-mzt-nxt-100.
      *              *-------------------------------------------------*
      *              * Read Next                                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       det-mzt-nxt-200.
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag di errore        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-det-mzt-nxt-flg
                     go to det-mzt-nxt-999.
       det-mzt-nxt-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-mzt-nxt-999.
       det-mzt-nxt-999.
           exit.

      *    *===========================================================*
      *    * Test max su [mmz] per determinazione saldi da [mmz]       *
      *    *-----------------------------------------------------------*
       det-mzt-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mzt-max-flg      .
       det-mzt-max-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se una o tutte le dipen-   *
      *              * denze                                           *
      *              *-------------------------------------------------*
           if        w-det-mzt-uot-dpz    =    "T"
                     go to det-mzt-max-600.
       det-mzt-max-200.
      *              *-------------------------------------------------*
      *              * Se una sola dipendenza                          *
      *              *-------------------------------------------------*
       det-mzt-max-250.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se un solo tipo archi- *
      *                  * vio o tutti i tipi archivio                 *
      *                  *---------------------------------------------*
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-max-500.
       det-mzt-max-300.
      *                  *---------------------------------------------*
      *                  * Se un solo tipo archivio                    *
      *                  *---------------------------------------------*
       det-mzt-max-325.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se uno o tutti i   *
      *                      * codici archivio                         *
      *                      *-----------------------------------------*
           if        w-det-mzt-uot-arc    =    "T"
                     go to det-mzt-max-450.
       det-mzt-max-350.
      *                      *-----------------------------------------*
      *                      * Se un solo codice archivio              *
      *                      *-----------------------------------------*
       det-mzt-max-375.
      *                          *-------------------------------------*
      *                          * Test su anno di esercizio           *
      *                          *-------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzt-sub-ese
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-380.
      *                          *-------------------------------------*
      *                          * Test su codice dipendenza           *
      *                          *-------------------------------------*
           if        rf-mmz-cod-dpz       not  = w-det-mzt-cod-dpz
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-385.
      *                          *-------------------------------------*
      *                          * Test su tipo archivio               *
      *                          *-------------------------------------*
           if        rf-mmz-tip-arc       not  = w-det-mzt-tip-arc
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-390.
      *                          *-------------------------------------*
      *                          * Test su codice archivio             *
      *                          *-------------------------------------*
           if        rf-mmz-cod-arc       not  = w-det-mzt-cod-arc
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-395.
      *                          *-------------------------------------*
      *                          * Test su codice dipendenza per il    *
      *                          * codice archivio                     *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-dpa    =    "T"
                     go to det-mzt-max-400.
           if        rf-mmz-dpz-arc       not  = w-det-mzt-dpz-arc
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-400.
      *                          *-------------------------------------*
      *                          * Test su tipo codice di magazzino    *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-dpa    =    "T"
                     go to det-mzt-max-405.
           if        rf-mmz-dpz-arc       not  = w-det-mzt-dpz-arc
                     go to det-mzt-max-405.
           if        rf-mmz-tip-mag       not  = w-det-mzt-tip-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-405.
      *                          *-------------------------------------*
      *                          * Test su codice di magazzino         *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-dpa    =    "T"
                     go to det-mzt-max-410.
           if        rf-mmz-dpz-arc       not  = w-det-mzt-dpz-arc
                     go to det-mzt-max-410.
           if        rf-mmz-tip-mag       not  = w-det-mzt-tip-mag
                     go to det-mzt-max-410.
           if        rf-mmz-num-mag       not  = w-det-mzt-num-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-410.
      *                          *-------------------------------------*
      *                          * Test su codice variante             *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-dpa    =    "T"
                     go to det-mzt-max-415.
           if        rf-mmz-dpz-arc       not  = w-det-mzt-dpz-arc
                     go to det-mzt-max-415.
           if        rf-mmz-tip-mag       not  = w-det-mzt-tip-mag
                     go to det-mzt-max-415.
           if        rf-mmz-num-mag       not  = w-det-mzt-num-mag
                     go to det-mzt-max-415.
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-415.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-415.
      *                          *-------------------------------------*
      *                          * Test su tipo conto merce            *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-dpa    =    "T"
                     go to det-mzt-max-420.
           if        rf-mmz-dpz-arc       not  = w-det-mzt-dpz-arc
                     go to det-mzt-max-420.
           if        rf-mmz-tip-mag       not  = w-det-mzt-tip-mag
                     go to det-mzt-max-420.
           if        rf-mmz-num-mag       not  = w-det-mzt-num-mag
                     go to det-mzt-max-420.
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-420.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-420.
           if        rf-mmz-tip-ctm       not  = "T"
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-420.
      *                          *-------------------------------------*
      *                          * Test su codice conto merce          *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-dpa    =    "T"
                     go to det-mzt-max-425.
           if        rf-mmz-dpz-arc       not  = w-det-mzt-dpz-arc
                     go to det-mzt-max-425.
           if        rf-mmz-tip-mag       not  = w-det-mzt-tip-mag
                     go to det-mzt-max-425.
           if        rf-mmz-num-mag       not  = w-det-mzt-num-mag
                     go to det-mzt-max-425.
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-425.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-425.
           if        rf-mmz-tip-ctm       not  = "T"
                     go to det-mzt-max-425.
           if        w-det-mzt-uot-ctm    =    "T"
                     go to det-mzt-max-425.
           if        rf-mmz-cod-ctm       not  = w-det-mzt-cod-ctm
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-425.
      *                          *-------------------------------------*
      *                          * Uscita per test superato            *
      *                          *-------------------------------------*
           go to     det-mzt-max-999.
       det-mzt-max-450.
      *                      *-----------------------------------------*
      *                      * Se tutti i codici archivio              *
      *                      *-----------------------------------------*
       det-mzt-max-455.
      *                          *-------------------------------------*
      *                          * Test su anno di esercizio           *
      *                          *-------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzt-sub-ese
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-460.
      *                          *-------------------------------------*
      *                          * Test su codice dipendenza           *
      *                          *-------------------------------------*
           if        rf-mmz-cod-dpz       not  = w-det-mzt-cod-dpz
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-465.
      *                          *-------------------------------------*
      *                          * Test su tipo codice di magazzino    *
      *                          *-------------------------------------*
           if        rf-mmz-tip-mag       not  = w-det-mzt-tip-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-470.
      *                          *-------------------------------------*
      *                          * Test su codice di magazzino         *
      *                          *-------------------------------------*
           if        rf-mmz-num-mag       not  = w-det-mzt-num-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-475.
      *                          *-------------------------------------*
      *                          * Test su codice variante             *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-480.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-480.
      *                          *-------------------------------------*
      *                          * Test su tipo conto merce            *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-485.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-485.
           if        rf-mmz-tip-ctm       not  = "T"
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-485.
      *                          *-------------------------------------*
      *                          * Test su codice conto merce          *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-490.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-490.
           if        rf-mmz-tip-ctm       not  = "T"
                     go to det-mzt-max-490.
           if        w-det-mzt-uot-ctm    =    "T"
                     go to det-mzt-max-490.
           if        rf-mmz-cod-ctm       not  = w-det-mzt-cod-ctm
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-490.
      *                          *-------------------------------------*
      *                          * Test su tipo archivio               *
      *                          *-------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-495.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-495.
           if        rf-mmz-tip-ctm       not  = "T"
                     go to det-mzt-max-495.
           if        w-det-mzt-uot-ctm    =    "T"
                     go to det-mzt-max-495.
           if        rf-mmz-cod-ctm       not  = w-det-mzt-cod-ctm
                     go to det-mzt-max-495.
           if        rf-mmz-tip-arc       not  = w-det-mzt-tip-arc
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-495.
      *                          *-------------------------------------*
      *                          * Uscita per test superato            *
      *                          *-------------------------------------*
           go to     det-mzt-max-999.
       det-mzt-max-500.
      *                  *---------------------------------------------*
      *                  * Se tutti i tipo archivio                    *
      *                  *---------------------------------------------*
       det-mzt-max-505.
      *                      *-----------------------------------------*
      *                      * Test su anno di esercizio               *
      *                      *-----------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzt-sub-ese
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-510.
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza               *
      *                      *-----------------------------------------*
           if        rf-mmz-cod-dpz       not  = w-det-mzt-cod-dpz
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-515.
      *                      *-----------------------------------------*
      *                      * Test su tipo codice di magazzino        *
      *                      *-----------------------------------------*
           if        rf-mmz-tip-mag       not  = w-det-mzt-tip-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-520.
      *                      *-----------------------------------------*
      *                      * Test su codice di magazzino             *
      *                      *-----------------------------------------*
           if        rf-mmz-num-mag       not  = w-det-mzt-num-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-525.
      *                      *-----------------------------------------*
      *                      * Test su codice variante                 *
      *                      *-----------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-530.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-530.
      *                      *-----------------------------------------*
      *                      * Test su tipo conto merce                *
      *                      *-----------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-535.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-535.
           if        rf-mmz-tip-ctm       not  = "T"
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-535.
      *                      *-----------------------------------------*
      *                      * Test su codice conto merce              *
      *                      *-----------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-540.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-540.
           if        rf-mmz-tip-ctm       not  = "T"
                     go to det-mzt-max-540.
           if        w-det-mzt-uot-ctm    =    "T"
                     go to det-mzt-max-540.
           if        rf-mmz-cod-ctm       not  = w-det-mzt-cod-ctm
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-540.
      *                      *-----------------------------------------*
      *                      * Uscita per test superato                *
      *                      *-----------------------------------------*
           go to     det-mzt-max-999.
       det-mzt-max-600.
      *              *-------------------------------------------------*
      *              * Se tutte le dipendenza                          *
      *              *-------------------------------------------------*
       det-mzt-max-605.
      *                  *---------------------------------------------*
      *                  * Test su anno di esercizio                   *
      *                  *---------------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzt-sub-ese
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-610.
      *                  *---------------------------------------------*
      *                  * Test su tipo codice di magazzino            *
      *                  *---------------------------------------------*
           if        rf-mmz-tip-mag       not  = w-det-mzt-tip-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-615.
      *                  *---------------------------------------------*
      *                  * Test su codice di magazzino                 *
      *                  *---------------------------------------------*
           if        rf-mmz-num-mag       not  = w-det-mzt-num-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-620.
      *                  *---------------------------------------------*
      *                  * Test su codice variante                     *
      *                  *---------------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-625.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-625.
      *                  *---------------------------------------------*
      *                  * Test su tipo conto merce                    *
      *                  *---------------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-630.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-630.
           if        rf-mmz-tip-ctm       not  = "T"
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-630.
      *                  *---------------------------------------------*
      *                  * Test su codice conto merce                  *
      *                  *---------------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-635.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-635.
           if        rf-mmz-tip-ctm       not  = "T"
                     go to det-mzt-max-635.
           if        w-det-mzt-uot-ctm    =    "T"
                     go to det-mzt-max-635.
           if        rf-mmz-cod-ctm       not  = w-det-mzt-cod-ctm
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-635.
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-640.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-640.
           if        rf-mmz-tip-ctm       not  = "T"
                     go to det-mzt-max-640.
           if        w-det-mzt-uot-ctm    =    "T"
                     go to det-mzt-max-640.
           if        rf-mmz-cod-ctm       not  = w-det-mzt-cod-ctm
                     go to det-mzt-max-640.
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-max-640.
           if        rf-mmz-tip-arc       not  = w-det-mzt-tip-arc
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-640.
      *                  *---------------------------------------------*
      *                  * Test su codice archivio                     *
      *                  *---------------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-645.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-645.
           if        rf-mmz-tip-ctm       not  = "T"
                     go to det-mzt-max-645.
           if        w-det-mzt-uot-ctm    =    "T"
                     go to det-mzt-max-645.
           if        rf-mmz-cod-ctm       not  = w-det-mzt-cod-ctm
                     go to det-mzt-max-645.
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-max-645.
           if        rf-mmz-tip-arc       not  = w-det-mzt-tip-arc
                     go to det-mzt-max-645.
           if        w-det-mzt-uot-arc    =    "T"
                     go to det-mzt-max-645.
           if        rf-mmz-cod-arc       not  = w-det-mzt-cod-arc
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-645.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza per il codice ar- *
      *                  * chivio                                      *
      *                  *---------------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-max-650.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     go to det-mzt-max-650.
           if        rf-mmz-tip-ctm       not  = "T"
                     go to det-mzt-max-650.
           if        w-det-mzt-uot-ctm    =    "T"
                     go to det-mzt-max-650.
           if        rf-mmz-cod-ctm       not  = w-det-mzt-cod-ctm
                     go to det-mzt-max-650.
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-max-650.
           if        rf-mmz-tip-arc       not  = w-det-mzt-tip-arc
                     go to det-mzt-max-650.
           if        w-det-mzt-uot-arc    =    "T"
                     go to det-mzt-max-650.
           if        rf-mmz-cod-arc       not  = w-det-mzt-cod-arc
                     go to det-mzt-max-650.
           if        w-det-mzt-uot-dpa    =    "T"
                     go to det-mzt-max-650.
           if        rf-mmz-dpz-arc       not  = w-det-mzt-dpz-arc
                     move  "#"            to   w-det-mzt-max-flg
                     go to det-mzt-max-999.
       det-mzt-max-650.
      *                  *---------------------------------------------*
      *                  * Uscita per test superato                    *
      *                  *---------------------------------------------*
           go to     det-mzt-max-999.
       det-mzt-max-999.
           exit.

      *    *===========================================================*
      *    * Selezione su [mmz] per determinazione saldi da [mmz]      *
      *    *-----------------------------------------------------------*
       det-mzt-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-mzt-sel-flg      .
       det-mzt-sel-100.
      *              *-------------------------------------------------*
      *              * Selezione su anno di esercizio                  *
      *              *-------------------------------------------------*
           if        rf-mmz-ann-ese       not  = w-det-mzt-sub-ese
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-150.
      *              *-------------------------------------------------*
      *              * Selezione su codice dipendenza                  *
      *              *-------------------------------------------------*
           if        w-det-mzt-uot-dpz    =    "T"
                     go to det-mzt-sel-200.
           if        rf-mmz-cod-dpz       not  = w-det-mzt-cod-dpz
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-200.
      *              *-------------------------------------------------*
      *              * Selezione su tipo codice di magazzino           *
      *              *-------------------------------------------------*
           if        rf-mmz-tip-mag       not  = w-det-mzt-tip-mag
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-250.
      *              *-------------------------------------------------*
      *              * Selezione su codice numerico di magazzino       *
      *              *-------------------------------------------------*
           if        rf-mmz-num-mag       not  = w-det-mzt-num-mag
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-300.
      *              *-------------------------------------------------*
      *              * Selezione su codice variante                    *
      *              *-------------------------------------------------*
           if        w-det-mzt-uot-var    =    "T"
                     go to det-mzt-sel-350.
           if        rf-mmz-var-mag       not  = w-det-mzt-var-mag
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-350.
      *              *-------------------------------------------------*
      *              * Selezione su tipo conto merce                   *
      *              *-------------------------------------------------*
           if        rf-mmz-tip-ctm       not  = "T"
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-400.
      *              *-------------------------------------------------*
      *              * Selezione su codice conto merce                 *
      *              *-------------------------------------------------*
           if        w-det-mzt-uot-ctm    =    "T"
                     go to det-mzt-sel-450.
           if        rf-mmz-cod-ctm       not  = w-det-mzt-cod-ctm
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-450.
      *              *-------------------------------------------------*
      *              * Selezione su tipo archivio                      *
      *              *-------------------------------------------------*
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-sel-500.
           if        rf-mmz-tip-arc       not  = w-det-mzt-tip-arc
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-500.
      *              *-------------------------------------------------*
      *              * Selezione su codice archivio                    *
      *              *-------------------------------------------------*
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-sel-550.
           if        w-det-mzt-uot-arc    =    "T"
                     go to det-mzt-sel-550.
           if        rf-mmz-cod-arc       not  = w-det-mzt-cod-arc
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-550.
      *              *-------------------------------------------------*
      *              * Selezione su codice dipendenza relativo al co-  *
      *              * dice archivio                                   *
      *              *-------------------------------------------------*
           if        w-det-mzt-tip-arc    =    spaces
                     go to det-mzt-sel-600.
           if        w-det-mzt-uot-arc    =    "T"
                     go to det-mzt-sel-600.
           if        w-det-mzt-uot-dpa    =    "T"
                     go to det-mzt-sel-600.
           if        rf-mmz-dpz-arc       not  = w-det-mzt-dpz-arc
                     move  "#"            to   w-det-mzt-sel-flg
                     go to det-mzt-sel-999.
       det-mzt-sel-600.
      *              *-------------------------------------------------*
      *              * Uscita per selezione superata                   *
      *              *-------------------------------------------------*
           go to     det-mzt-sel-999.
       det-mzt-sel-999.
           exit.

