       Identification Division.
       Program-Id.                                 pmag300z           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mag                 *
      *                                Settore:    mov                 *
      *                                   Fase:    mag300z             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/12/89    *
      *                       Ultima revisione:    NdK del 10/03/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Programma di interfaccia movimenti di MAG   *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "OP"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-mag-300-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "NT" - Normalizzazione work-area testata e riga movimento      *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "NT"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "NR" - Normalizzazione work-area sola riga movimento           *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "NR"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "WT" - Scrittura testata movimento di magazzino                *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "WT"                       *
      *                 l-mag-300-dat-reg = data registrazione         *
      *                 l-mag-300-num-prt = zero se deve essere pre-   *
      *                                     levato automaticamente ;   *
      *                                     altrimenti il numero pro-  *
      *                                     tocollo da utilizzare      *
      *                 l-mag-300-cod-dpz = codice dipendenza          *
      *                 l-mag-300-cod-cau = codice causale di magazzi- *
      *                                     no                         *
      *                 l-mag-300-cod-ctm = codice conto merce         *
      *                 l-mag-300-dat-doc = opzionale                  *
      *                 l-mag-300-num-doc = opzionale                  *
      *                 l-mag-300-cod-dsl = codice dislocazione        *
      *                 l-mag-300-cod-dsd = codice dislocazione di de- *
      *                                     stinazione (obbligatorio   *
      *                                     se causale di magazzino di *
      *                                     'Movimentazione interna'   *
      *                 l-mag-300-tip-arc = tipo archivio : obbligato- *
      *                                     rio                        *
      *                                     - N : no tipo archivio     *
      *                                     - C : cliente              *
      *                                     - F : fornitore            *
      *                                     - D : dipendenza           *
      *                                     - A : agente               *
      *                 l-mag-300-cod-arc = codice archivio : obbliga- *
      *                                     torio tranne che per tipo  *
      *                                     archivio 'N'               *
      *                 l-mag-300-dpz-arc = facoltativo                *
      *                                                                *
      *        Output : l-mag-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "WR" - Scrittura riga movimento di magazzino                   *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "WR"                       *
      *                 l-mag-300-tip-mag = tipo codice di magazzino : *
      *                                     - 01 : Prodotti di vendita *
      *                                     - 02 : Semilavorati        *
      *                                     - 03 : Materie prime       *
      *                                     - 04 : Materiali vari      *
      *                                     (obbligatorio)             *
      *                 l-mag-300-num-mag = codice numerico magazzino  *
      *                                     (obbligatorio)             *
      *                 l-mag-300-alf-mag = codice alfanumerico magaz- *
      *                                     zino; (obbligatorio)       *
      *                 l-mag-300-var-mag = Sigla della variante       *
      *                                     a spaces in quanto non ge- *
      *                                     stita attualmente          *
      *                 l-mag-300-udm-ana = Unita' di misura da anagra-*
      *                                     fica magazzino; (obbligato-*
      *                                     rio)                       *
      *                 l-mag-300-udm-mov = Unita' di misura per il mo-*
      *                                     vimento; se a spaces viene *
      *                                     forzato quello dell'anagra *
      *                                     fica magazzino             *
      *                 l-mag-300-num-pmg = Numero partita di magazzino*
      *                                     a zero in quanto non ge-   *
      *                                     stita attualmente          *
      *                 l-mag-300-ddo-rif = Data documento di riferi-  *
      *                                     mento; non gestita attual  *
      *                                     mente                      *
      *                 l-mag-300-ndo-rif = Num. documento di riferi-  *
      *                                     mento; non gestito attual  *
      *                                     mente                      *
      *                 l-mag-300-qta-mov = Quantita' movimento        *
      *                                     (obbligatorio se richiesto *
      *                                     dal trattamento valore as- *
      *                                     sociato alla causale)      *
      *                 l-mag-300-cun-mov = Costo unitario movimento   *
      *                                     (facoltativo)              *
      *                 l-mag-300-val-mov = Valore movimento           *
      *                                     (obbligatorio se richiesto *
      *                                     dal trattamento valore as- *
      *                                     sociato alla causale)      *
      *                                                                *
      *        Output : l-mag-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "TI" - Test se un movimento di mag e' inseribile               *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "TI"                       *
      *                 l-mag-300-dat-reg = obbligatorio               *
      *                 l-mag-300-cod-cau = obbligatorio               *
      *                                                                *
      *        Output : l-mag-300-exi-sts = Spaces : movimento inse-   *
      *                                              ribile            *
      *                                     #      : movimento non     *
      *                                              inseribile con    *
      *                                              la data di re-    *
      *                                              gistrazione e la  *
      *                                              causale passati   *
      *                                              in input          *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "TM" - Test se un movimento di mag e' modificabile o annulla-  *
      *        bile                                                    *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "TM"                       *
      *                 l-mag-300-dat-reg = obbligatorio               *
      *                 l-mag-300-num-prt = obbligatorio               *
      *                                                                *
      *        Output : l-mag-300-exi-sts = Spaces: movimento modifi-  *
      *                                     cabile interamente o an-   *
      *                                     nullabile                  *
      *                                     #     : non modificabile   *
      *                                             ne' annullabile    *
      *                                     V     : modificabile solo  *
      *                                             nei valori e non   *
      *                                             annullabile        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "TE" - Test se un movimento di mag e' esistente                *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "TE"                       *
      *                 l-mag-300-dat-reg = obbligatorio               *
      *                 l-mag-300-num-prt = obbligatorio               *
      *                                                                *
      *        Output : l-mag-300-exi-sts = Spaces : movimento esis-   *
      *                                              tente             *
      *                                     #      : movimento non     *
      *                                              esistente         *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "NP" - Richiesta numero protocollo di mag                      *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "NP"                       *
      *                 l-mag-300-dat-reg = obbligatorio               *
      *                 l-mag-300-cod-dpz = obbligatorio               *
      *                                                                *
      *        Output : l-mag-300-num-prt = Nuovo numero protocollo    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "DE" - Delete movimento di mag                                 *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "DE"                       *
      *                 l-mag-300-dat-reg = obbligatorio               *
      *                 l-mag-300-num-prt = obbligatorio               *
      *                                                                *
      *        Output : l-mag-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "DR" - Richiesta data registrazione minima di mag              *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "DR"                       *
      *                                                                *
      *        Output : l-mag-300-dat-reg = data registrazione minima  *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "UM" - Ultimo movimento di mag per un prodotto                 *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "UM"                       *
      *                 l-mag-300-dat-reg = facoltativo                *
      *                 l-mag-300-tip-mag = tipo codice di magazzino   *
      *                                     (obbligatorio)             *
      *                 l-mag-300-num-mag = codice numerico magazzino  *
      *                                     (obbligatorio)             *
      *                 l-mag-300-cod-cau = causale di magazzino       *
      *                                     (facoltativa)              *
      
      *        Output : l-mag-300-num-prt = protocollo individuato     *
      *                 l-mag-300-dat-reg = data registrazione         *
      *                 l-mag-300-num-doc = numero documento           *
      *                 l-mag-300-dat-doc = data registrazione         *
      *                                                                *
      *                 l-mag-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "CD" - Cambio dislocazione (prodotti di vendita)               *
      *       (Forzatura Dipendenza e tipo magazzino)                  *
      *                                                                *
      *        Input  : l-mag-300-tip-ope = "CD"                       *
      *                                                                *
      *                 l-mag-300-dat-reg = data registrazione         *
      *                                     (obbligatorio)             *
      *                 l-mag-300-num-prt = zero se deve essere pre-   *
      *                                     levato automaticamente ;   *
      *                                     altrimenti il numero pro-  *
      *                                     tocollo da utilizzare      *
      *                 l-mag-300-cod-cau = codice causale magazzino   *
      *                                     (obbligatorio)             *
      *                 l-mag-300-dat-doc = opzionale                  *
      *                 l-mag-300-num-doc = opzionale                  *
      *                                                                *
      *                 l-mag-300-num-mag = codice numerico prodotto   *
      *                                     (obbligatorio)             *
      *                 l-mag-300-alf-mag = codice alfanumerico        *
      *                                     (obbligatorio)             *
      *                 l-mag-300-qta-mov = Quantita' movimento        *
      *                                     (obbligatorio)             *
      *                 l-mag-300-cod-dsl = codice dislocazione        *
      *                                     (obbligatorio)             *
      *                 l-mag-300-cod-dsd = codice dislocazione di de- *
      *                                     stinazione                 *
      *                                     (obbligatorio)             *
      *                                                                *
      *        Output : l-mag-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
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
      *    * Record file                                               *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mmt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmt"                          .
      *        *-------------------------------------------------------*
      *        * [mmr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmr"                          .
      *        *-------------------------------------------------------*
      *        * [mms]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmms"                          .
      *        *-------------------------------------------------------*
      *        * [mmz]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmz"                          .
      *        *-------------------------------------------------------*
      *        * [mmv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmv"                          .
      *        *-------------------------------------------------------*
      *        * [zmc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmc"                          .

      *    *===========================================================*
      *    * Record file numerazioni                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * Record numerazione data inventario           [datife] *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/num/rec/rndatife"                       .
      *        *-------------------------------------------------------*
      *        * Record numerazione data ultimo consolidamento costo   *
      *        * medio continuo                               [duccmc] *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/num/rec/rnduccmc"                       .
      *        *-------------------------------------------------------*
      *        * Record numerazione numero protocollo di mag  [prtmag] *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/num/rec/rnprtmag"                       .

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
      *        *-------------------------------------------------------*
      *        * Da record numerazioni [duccmc]                        *
      *        *-------------------------------------------------------*
           05  w-dnu-cmc.
      *            *---------------------------------------------------*
      *            * Data apertura periodo inventario fine esercizio   *
      *            *---------------------------------------------------*
               10  w-dnu-cmc-dat-con      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Flag di data registrazione minima determinata         *
      *        *-------------------------------------------------------*
           05  w-nol-dtr-flg              pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Data registrazione minima                             *
      *        *-------------------------------------------------------*
           05  w-nol-dtr-min              pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area di comodo                                       *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Contatore di riga movimento                           *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-rig              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 'I'                               *
      *        *-------------------------------------------------------*
           05  I                          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Data registrazione di comodo                          *
      *        *-------------------------------------------------------*
           05  w-wrk-dat-reg              pic  9(07)                  .

      *    *===========================================================*
      *    * Work per Bufferizzazione record [zmc]                     *
      *    *-----------------------------------------------------------*
       01  w-zmc.
           05  w-zmc-cod-cau              pic  9(05)                  .
           05  w-zmc-des-cau              pic  x(30)                  .
           05  w-zmc-des-key              pic  x(30)                  .
           05  w-zmc-mne-cau              pic  x(05)                  .
           05  w-zmc-com-cau              pic  x(30)                  .
           05  w-zmc-trt-val              pic  x(01)                  .
           05  w-zmc-tip-mdm              pic  9(02)                  .
           05  w-zmc-trt-mic              pic  9(02)                  .
           05  w-zmc-tip-mic              pic  x(01)                  .
           05  w-zmc-cod-mic              pic  x(03)                  .
           05  w-zmc-snv-mic              pic  x(01)                  .
           05  w-zmc-def-tar              pic  x(01)                  .
           05  w-zmc-snv-tar              pic  x(01)                  .
           05  w-zmc-lst-tar              pic  x(04)                  .
           05  w-zmc-def-tco              pic  x(01)                  .
           05  w-zmc-snv-tco              pic  x(01)                  .
           05  w-zmc-lst-tco              pic  x(04)                  .
           05  w-zmc-alx-exp.
               10  filler  occurs 50      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Agg                               *
      *    *-----------------------------------------------------------*
       01  w-agg.
      *        *-------------------------------------------------------*
      *        * Work per aggiornamenti gestione magazzino             *
      *        *-------------------------------------------------------*
           05  w-agg-mag.
      *            *---------------------------------------------------*
      *            * Tipo aggiornamento                                *
      *            * - + : In piu'                                     *
      *            * - - : In meno                                     *
      *            *---------------------------------------------------*
               10  w-agg-mag-tip-agg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Data di registrazione del movimento               *
      *            *---------------------------------------------------*
               10  w-agg-mag-dat-reg      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Esercizio per inventario di fine anno : s.aa      *
      *            *---------------------------------------------------*
               10  w-agg-mag-ese-inv      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Esercizio di riferimento : s.aa                   *
      *            *                                                   *
      *            * Calcolato automaticamente dalla routine di ag-    *
      *            * giornamento in funzione della data di registra-   *
      *            * zione                                             *
      *            *---------------------------------------------------*
               10  w-agg-mag-ann-ese      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Mese nell'esercizio di riferimento : 01..12       *
      *            *                                                   *
      *            * Calcolato automaticamente dalla routine di ag-    *
      *            * giornamento in funzione della data di registra-   *
      *            * zione                                             *
      *            *---------------------------------------------------*
               10  w-agg-mag-mes-ese      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Mese di chiusura esercizio, calcolato automatica  *
      *            * mente dalla routine di aggiornamento alla prima   *
      *            * esecuzione della routine stessa                   *
      *            *---------------------------------------------------*
               10  w-agg-mag-mes-che      pic  9(02)       value zero .
      *            *---------------------------------------------------*
      *            * Tipo di trattamento per il valore                 *
      *            * - N : Nessun trattamento del valore               *
      *            * - I : Trattamento del valore per impostazione     *
      *            * - S : Trattamento del valore, ma senza imposta-   *
      *            *       zione in quanto valorizzazione al costo     *
      *            *       standard                                    *
      *            * - C : Trattamento del solo valore, senza quanti-  *
      *            *       ta' come costo aggiuntivo                   *
      *            * - X : Trattamento del solo valore, senza quanti-  *
      *            *       ta' come costo aggiuntivo riferito alla fi- *
      *            *       ne esercizio precedente                     *
      *            * - R : Trattamento del solo valore, senza quanti-  *
      *            *       ta' come rettifica ad un costo              *
      *            * - Y : Trattamento del solo valore, senza quanti-  *
      *            *       ta' come rettifica ad un costo riferito al- *
      *            *       la fine esercizio precedente                *
      *            *---------------------------------------------------*
               10  w-agg-mag-trt-val      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo movimentazione di magazzino                  *
      *            * - 01 : Carico                                     *
      *            * - 02 : Scarico                                    *
      *            * - 03 : Movimentazione interna                     *
      *            *---------------------------------------------------*
               10  w-agg-mag-tip-mdm      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Trattamento conto merce                           *
      *            * - 01 : Puo' riferirsi ad un altro documento il    *
      *            *        quale era un documento di movimentazione   *
      *            *        di merce in conto                          *
      *            * - 02 : Non puo' riferirsi ad un altro documento   *
      *            *        di movimentazione di merce in conto        *
      *            * - 03 : Il documento stesso e' una movimentazione  *
      *            *        di merce in conto                          *
      *            *---------------------------------------------------*
               10  w-agg-mag-trt-mic      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo conto merce                                  *
      *            * - Spaces : Non significativo                      *
      *            * - N      : Merce presso terzi ma non di loro pro- *
      *            *            prieta'                                *
      *            * - T      : Merce presso di noi ma non di nostra   *
      *            *            proprieta'                             *
      *            *---------------------------------------------------*
               10  w-agg-mag-tip-mic      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice conto merce, significativo solo se tipo    *
      *            * conto merce pari a 'N' o 'T'                      *
      *            *---------------------------------------------------*
               10  w-agg-mag-cod-mic      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Tipo archivio di riferimento                      *
      *            * - Spaces : Non significativo                      *
      *            * - C      : Cliente                                *
      *            * - F      : Fornitore                              *
      *            * - A      : Agente                                 *
      *            * Significativo solamente per l'aggiornamento re-   *
      *            * lativo alla merce in conto                        *
      *            *---------------------------------------------------*
               10  w-agg-mag-tip-arc      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice archivio di riferimento                    *
      *            * - Se cliente     : codice su [cli]                *
      *            * - Se fornitore   : codice su [fnt]                *
      *            * - Se agente      : codice su [age]                *
      *            * Significativo solamente per l'aggiornamento re-   *
      *            * lativo alla merce in conto                        *
      *            *---------------------------------------------------*
               10  w-agg-mag-cod-arc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza per il codice archivio di rife- *
      *            * rimento                                           *
      *            * - Se cliente     : codice dipendenza su [dcc]     *
      *            * - Se fornitore   : codice dipendenza su [dcf]     *
      *            * - Se agente      : non significativo              *
      *            * Significativo solamente per l'aggiornamento re-   *
      *            * lativo alla merce in conto                        *
      *            *---------------------------------------------------*
               10  w-agg-mag-dpz-arc      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza di riferimento                  *
      *            *---------------------------------------------------*
               10  w-agg-mag-cod-dpz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo codice di magazzino                          *
      *            * - 01 : Prodotto di vendita                        *
      *            * - 02 : Semilavorato                               *
      *            * - 03 : Materia prima                              *
      *            * - 04 : Materiale vario                            *
      *            *---------------------------------------------------*
               10  w-agg-mag-tip-mag      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico di magazzino                      *
      *            * - Se prodotto di vendita   : codice su [dcp]      *
      *            * - Se semilavorato          : codice su [dps]      *
      *            * - Se materia prima         : codice su [dpm]      *
      *            * - Se materiale vario       : codice su [mtv]      *
      *            *---------------------------------------------------*
               10  w-agg-mag-num-mag      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Sigla della variante per il codice numerico       *
      *            *---------------------------------------------------*
               10  w-agg-mag-var-mag      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Codice dislocazione                               *
      *            *---------------------------------------------------*
               10  w-agg-mag-cod-dsl      pic  x(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dislocazione di destinazione               *
      *            *---------------------------------------------------*
               10  w-agg-mag-cod-dsd      pic  x(07)                  .
      *            *---------------------------------------------------*
      *            * Codice unita' di misura da anagrafica             *
      *            *---------------------------------------------------*
               10  w-agg-mag-udm-ana      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Codice unita' di misura per il movimento          *
      *            *---------------------------------------------------*
               10  w-agg-mag-udm-mov      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Quantita' del movimento                           *
      *            *---------------------------------------------------*
               10  w-agg-mag-qta-mov      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Costo unitario del movimento                      *
      *            *---------------------------------------------------*
               10  w-agg-mag-cun-mov      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Valore movimento                                  *
      *            *---------------------------------------------------*
               10  w-agg-mag-val-mov      pic s9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di attribuzione numero protocollo    *
      *    *-----------------------------------------------------------*
       01  w-prt.
      *        *-------------------------------------------------------*
      *        * Work per protocollo di gestione magazzino 'mag'       *
      *        *-------------------------------------------------------*
           05  w-prt-mag.
               10  w-prt-mag-ann-ese      pic  9(03)                  .
               10  w-prt-mag-cod-dpz      pic  9(02)                  .
               10  w-prt-mag-num-prt      pic  9(11)                  .
               10  w-prt-mag-r01-prt redefines
                   w-prt-mag-num-prt.
                   15  w-prt-mag-r01-ese  pic  9(03)                  .
                   15  w-prt-mag-r01-dpz  pic  9(02)                  .
                   15  w-prt-mag-r01-num  pic  9(06)                  .
               10  w-prt-mag-val-pre      pic  9(06)                  .
               10  w-prt-mag-val-pos      pic  9(06)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni generali                      *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazione per gestione dipendenze parallele   *
      *        *-------------------------------------------------------*
           05  w-prs-snx-gdp.
      *            *---------------------------------------------------*
      *            * Si/No gestione dipendenze parallele               *
      *            *---------------------------------------------------*
               10  w-prs-snx-gdp-snx      pic  x(01)     value spaces .

      *    *===========================================================*
      *    * Work-area per operazioni sulle date                       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per movimento di magazzino          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/pmag300z.pgl"                   .

      ******************************************************************
       Procedure Division                using l-mag-300              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   l-mag-300-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Write testata movimento di magazzino        *
      *                  *---------------------------------------------*
           if        l-mag-300-tip-ope    =    "WT"
                     perform wte-000      thru wte-999
      *                  *---------------------------------------------*
      *                  * Write righe movimento di magazzino          *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "WR"
                     perform wrg-000      thru wrg-999
      *                  *---------------------------------------------*
      *                  * Rilascio numero protocollo di mag           *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "NP"
                     perform rnp-000      thru rnp-999
      *                  *---------------------------------------------*
      *                  * Test se movimento inseribile                *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "TI"
                     perform tsi-000      thru tsi-999
      *                  *---------------------------------------------*
      *                  * Test se movimento modificabile o annullabi- *
      *                  * le                                          *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "TM"
                     perform tsm-000      thru tsm-999
      *                  *---------------------------------------------*
      *                  * Test se movimento esistente                 *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "TE"
                     perform tse-000      thru tse-999
      *                  *---------------------------------------------*
      *                  * Normalizzazione testata movimento           *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "NT"
                     perform nte-000      thru nte-999
      *                  *---------------------------------------------*
      *                  * Normalizzazione riga movimento              *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "NR"
                     perform nrg-000      thru nrg-999
      *                  *---------------------------------------------*
      *                  * Delete movimento                            *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "DE"
                     perform del-000      thru del-999
      *                  *---------------------------------------------*
      *                  * Richiesta data registrazione mag            *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "DR"
                     perform drm-000      thru drm-999
      *                  *---------------------------------------------*
      *                  * Ultimo movimento di mag per un prodotto     *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "UM"
                     perform umm-000      thru umm-999
      *                  *---------------------------------------------*
      *                  * Cambio dislocazione prodotto                *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "CD"
                     perform cdp-000      thru cdp-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-mag-300-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999                .
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
      *                  *---------------------------------------------*
      *                  * Apertura                                    *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *                  *---------------------------------------------*
      *                  * Spostamento in area di comodo               *
      *                  *---------------------------------------------*
           move      rn-dat-ife-flg-ife   to   w-dnu-ife-ife-flg      .
           move      rn-dat-ife-ini-ife   to   w-dnu-ife-ife-dti      .
           move      rn-dat-ife-fin-ife   to   w-dnu-ife-ife-dtf      .
           move      rn-dat-ife-ese-ife   to   w-dnu-ife-ife-ese      .
      *                  *---------------------------------------------*
      *                  * Chiusura                                    *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *              *-------------------------------------------------*
      *              * Lettura numerazione [duccmc]                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura                                    *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/induccmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-duc-cmc             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/induccmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-duc-cmc             .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/induccmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-duc-cmc             .
      *                  *---------------------------------------------*
      *                  * Spostamento in area di comodo               *
      *                  *---------------------------------------------*
           move      rn-duc-cmc-dat-con   to   w-dnu-cmc-dat-con      .
      *                  *---------------------------------------------*
      *                  * Chiusura                                    *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/induccmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-duc-cmc             .
      *              *-------------------------------------------------*
      *              * Apertura file principali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mmt] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmt                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mmr] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mms] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mmz] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mmv] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *              *-------------------------------------------------*
      *              * Apertura file ausiliari                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura file                         [zmc] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *              *-------------------------------------------------*
      *              * Apertura file numerazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura numerazione               [prtmag] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/inprtmag"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-mag             .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni generali              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No gestione dipendenze parallele         *
      *                  *---------------------------------------------*
           perform   prs-snx-gdp-000      thru prs-snx-gdp-999        .
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
      *              * Chiusura file principali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mmt] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmt                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mmr] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mms] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mmz] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mmv] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *              *-------------------------------------------------*
      *              * Chiusura file ausiliari                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [zmc] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *              *-------------------------------------------------*
      *              * Chiusura file numerazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura numerazione               [prtmag] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/inprtmag"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-mag             .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per Si/No gestione dipendenze   *
      *    * parallele                                                 *
      *    *-----------------------------------------------------------*
       prs-snx-gdp-000.
      *              *-------------------------------------------------*
      *              * Test se lettura necessaria                      *
      *              *-------------------------------------------------*
           if        w-prs-snx-gdp-snx    not  = spaces
                     go to prs-snx-gdp-999.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/azi[snx-gdp]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-gdp-snx
           else      move  spaces         to   w-prs-snx-gdp-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           if        w-prs-snx-gdp-snx    not  = "S"
                     move  "N"            to   w-prs-snx-gdp-snx      .
       prs-snx-gdp-999.
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
                     move  spaces         to   l-mag-300-exi-sts
           else      move  "#"            to   l-mag-300-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione link-area riga movimento                  *
      *    *-----------------------------------------------------------*
       nrg-000.
           move      zero                 to   l-mag-300-tip-mag      .
           move      zero                 to   l-mag-300-num-mag      .
           move      spaces               to   l-mag-300-alf-mag      .
           move      spaces               to   l-mag-300-var-mag      .
           move      spaces               to   l-mag-300-udm-ana      .
           move      spaces               to   l-mag-300-udm-mov      .
           move      zero                 to   l-mag-300-num-pmg      .
           move      zero                 to   l-mag-300-ddo-rif      .
           move      spaces               to   l-mag-300-ndo-rif      .
           move      zero                 to   l-mag-300-qta-mov      .
           move      zero                 to   l-mag-300-cun-mov      .
           move      zero                 to   l-mag-300-val-mov      .
       nrg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione link-area testata movimento               *
      *    *-----------------------------------------------------------*
       nte-000.
           move      zero                 to   l-mag-300-dat-reg      .
           move      zero                 to   l-mag-300-num-prt      .
           move      zero                 to   l-mag-300-cod-dpz      .
           move      zero                 to   l-mag-300-cod-cau      .
           move      spaces               to   l-mag-300-cod-ctm      .
           move      zero                 to   l-mag-300-dat-doc      .
           move      spaces               to   l-mag-300-num-doc      .
           move      spaces               to   l-mag-300-cod-dsl      .
           move      spaces               to   l-mag-300-cod-dsd      .
           move      spaces               to   l-mag-300-tip-arc      .
           move      zero                 to   l-mag-300-cod-arc      .
           move      spaces               to   l-mag-300-dpz-arc      .
       nte-999.
           exit.

      *    *===========================================================*
      *    * Scrittura testata movimento di magazzino                  *
      *    *-----------------------------------------------------------*
       wte-000.
      *              *-------------------------------------------------*
      *              * Lettura tabella causali di magazzino            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      l-mag-300-cod-cau    to   rf-zmc-cod-cau         .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *              *-------------------------------------------------*
      *              * Se esito lettura OK : oltre                     *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to wte-100.
      *                  *---------------------------------------------*
      *                  * Composizione messaggio                      *
      *                  *---------------------------------------------*
           move      "Aggiornamento di magazzino non effettuato: causale
      -              "di magazzino inesistente !"
                                          to   l-mag-300-msg-exi      .
      *                  *---------------------------------------------*
      *                  * Segnale di errore                           *
      *                  *---------------------------------------------*
           move      "#"                  to   l-mag-300-exi-sts      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     wte-999.
       wte-100.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori associati alla causale   *
      *              * di magazzino                                    *
      *              *-------------------------------------------------*
           move      rf-zmc-cod-cau       to   w-zmc-cod-cau          .
           move      rf-zmc-des-cau       to   w-zmc-des-cau          .
           move      rf-zmc-des-key       to   w-zmc-des-key          .
           move      rf-zmc-mne-cau       to   w-zmc-mne-cau          .
           move      rf-zmc-com-cau       to   w-zmc-com-cau          .
           move      rf-zmc-trt-val       to   w-zmc-trt-val          .
           move      rf-zmc-tip-mdm       to   w-zmc-tip-mdm          .
           move      rf-zmc-trt-mic       to   w-zmc-trt-mic          .
           move      rf-zmc-tip-mic       to   w-zmc-tip-mic          .
           move      rf-zmc-cod-mic       to   w-zmc-cod-mic          .
           move      rf-zmc-snv-mic       to   w-zmc-snv-mic          .
           move      rf-zmc-def-tar       to   w-zmc-def-tar          .
           move      rf-zmc-snv-tar       to   w-zmc-snv-tar          .
           move      rf-zmc-lst-tar       to   w-zmc-lst-tar          .
           move      rf-zmc-def-tco       to   w-zmc-def-tco          .
           move      rf-zmc-snv-tco       to   w-zmc-snv-tco          .
           move      rf-zmc-lst-tco       to   w-zmc-lst-tco          .
      *              *-------------------------------------------------*
      *              * Eventuale attribuzione numero protocollo        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se numero protocollo passato in link-area a *
      *                  * zero : oltre                                *
      *                  *---------------------------------------------*
           if        l-mag-300-num-prt    not  = zero
                     go to  wte-200.
      *                  *---------------------------------------------*
      *                  * Attribuzione numero protocollo              *
      *                  *---------------------------------------------*
           perform   rnp-000              thru rnp-999                .
       wte-200.
      *              *-------------------------------------------------*
      *              * Trattamento file [mmt]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Write record [mmt]                          *
      *                  *---------------------------------------------*
           perform   wrt-rec-mmt-000      thru wrt-rec-mmt-999        .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore per progressivo riga *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-rig          .
       wte-999.
           exit.

      *    *===========================================================*
      *    * Scrittura riga movimento di magazzino                     *
      *    *-----------------------------------------------------------*
       wrg-000.
      *              *-------------------------------------------------*
      *              * Trattamento file [mmr]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore numero riga movimento  *
      *                  *---------------------------------------------*
           add       00100                to   w-wrk-ctr-rig          .
      *                      *-----------------------------------------*
      *                      * Test se oltre il numero massimo consen- *
      *                      * tito                                    *
      *                      *-----------------------------------------*
           if        w-wrk-ctr-rig        not  < 99999
                     go to  wrg-900.
      *                  *---------------------------------------------*
      *                  * Write record [mmr]                          *
      *                  *---------------------------------------------*
           perform   wrt-rec-mmr-000      thru wrt-rec-mmr-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     wrg-999.
       wrg-900.
      *                      *-----------------------------------------*
      *                      * Se oltre il massimo numero di righe per *
      *                      * registrazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Registrazione con numero righe oltre il massimo co
      -              "nsentito !"         to   l-mag-300-msg-exi      .
      *                          *-------------------------------------*
      *                          * Delete intera registrazione         *
      *                          *-------------------------------------*
           perform   del-000              thru del-999                .
      *                          *-------------------------------------*
      *                          * Segnale di errore                   *
      *                          *-------------------------------------*
           move      "#"                  to   l-mag-300-exi-sts      .
       wrg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione data di registrazione minima               *
      *    *-----------------------------------------------------------*
       ddm-000.
      *              *-------------------------------------------------*
      *              * Se data gia' determinata : uscita               *
      *              *-------------------------------------------------*
           if        w-nol-dtr-flg        not  = spaces
                     go to ddm-999.
      *              *-------------------------------------------------*
      *              * Altrimenti : determinazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di data determinata                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-nol-dtr-flg          .
       ddm-500.
      *                  *---------------------------------------------*
      *                  * Data di registrazione minima la maggiore    *
      *                  * tra :                                       *
      *                  *                                             *
      *                  * - Giorno successivo alla data ultimo inven- *
      *                  *   tario di fine esercizio, sia esso ancora  *
      *                  *   in corso o gia' concluso                  *
      *                  *                                             *
      *                  * - Giorno successivo alla data ultimo conso- *
      *                  *   lidamento costo medio continuo            *
      *                  *                                             *
      *                  * ___ ATTENZIONE : ATTUALMENTE INIBITO ___    *
      *                  * ___ NdeK (24/08/95)                  ___    *
      *                  *                                             *
      *                  *---------------------------------------------*
           move      w-dnu-ife-ife-dti    to   w-nol-dtr-min          .
______*    if        w-dnu-cmc-dat-con    >    w-dnu-ife-ife-dti
______*              move  w-dnu-cmc-dat-con
______*                                   to   w-nol-dtr-min          .
           if        w-nol-dtr-min        =    zero
                     go to ddm-999.
           move      w-nol-dtr-min        to   s-dat                  .
           add       1                    to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to ddm-580.
           move      1                    to   s-gio                  .
           add       1                    to   s-mes                  .
           if        s-mes                not  > 12
                     go to ddm-580.
           move      1                    to   s-mes                  .
           add       1                    to   s-saa                  .
       ddm-580.
           move      s-dat                to   w-nol-dtr-min          .
       ddm-999.
           exit.

      *    *===========================================================*
      *    * Data registrazione minima di mag                          *
      *    *-----------------------------------------------------------*
       drm-000.
           perform   ddm-000              thru ddm-999                .
           move      w-nol-dtr-min        to   l-mag-300-dat-reg      .
       drm-999.
           exit.

      *    *===========================================================*
      *    * Ultimo movimento di mag per un prodotto                   *
      *    *-----------------------------------------------------------*
       umm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   l-mag-300-num-prt      .
       umm-050.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        l-mag-300-tip-mag    =    zero or
                     l-mag-300-num-mag    =    zero
                     go to umm-900.
       umm-070.
      *              *-------------------------------------------------*
      *              * Regolarizzazione della data registrazione       *
      *              * poter includere le fatture emesse il giorno     *
      *              * stesso                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se data registrazione a zero                *
      *                  *---------------------------------------------*
           if        l-mag-300-dat-reg    =    zero
                     move  9999999        to   w-wrk-dat-reg
                     go to umm-100.
      *                  *---------------------------------------------*
      *                  * Aumento di un giorno per la start a ritroso *
      *                  *---------------------------------------------*
           move      l-mag-300-dat-reg    to   w-det-dat-nrg-dtb      .
           move      1                    to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
           move      w-det-dat-nrg-dti    to   w-wrk-dat-reg          .
       umm-100.
      *              *-------------------------------------------------*
      *              * Start a ritroso su file [mmr]                   *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NG"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      l-mag-300-tip-mag    to   rf-mmr-tip-mag         .
           move      l-mag-300-num-mag    to   rf-mmr-num-mag         .
           move      w-wrk-dat-reg        to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to umm-900.
       umm-200.
      *              *-------------------------------------------------*
      *              * Previous su [mmr]                               *
      *              *-------------------------------------------------*
           move      "RP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to umm-900.
       umm-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
           if        rf-mmr-tip-mag       not  = l-mag-300-tip-mag or
                     rf-mmr-num-mag       not  = l-mag-300-num-mag
                     go to  umm-900.
       umm-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Su eventuale causale                        *
      *                  *---------------------------------------------*
           if        l-mag-300-cod-cau    =    zero
                     go to umm-500.
           if        rf-mmr-cod-cau       not  = l-mag-300-cod-cau
                     go to umm-200.
       umm-500.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori in output                *
      *              *-------------------------------------------------*
           move      rf-mmr-num-prt       to   l-mag-300-num-prt      .
           move      rf-mmr-dat-reg       to   l-mag-300-dat-reg      .
           move      rf-mmr-num-doc       to   l-mag-300-num-doc      .
           move      rf-mmr-dat-doc       to   l-mag-300-dat-doc      .
       umm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     umm-999.
       umm-999.
           exit.

      *    *===========================================================*
      *    * Test se movimento di 'MAG' inseribile                     *
      *    *-----------------------------------------------------------*
       tsi-000.
      *              *-------------------------------------------------*
      *              * Test su data registrazione minima               *
      *              *-------------------------------------------------*
           perform   ddm-000              thru ddm-999                .
           if        l-mag-300-dat-reg    <    w-nol-dtr-min
                     move   "#"           to   l-mag-300-exi-sts
                     go to  tsi-999.
      *              *-------------------------------------------------*
      *              * Controllo compatibilita' trattamento valore as- *
      *              * sociato alla causale con data registrazione e   *
      *              * dati inventario di fine esercizio               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se causale a zero : a uscita                *
      *                  *---------------------------------------------*
           if        l-mag-300-cod-cau    =    zero
                     go to tsi-200.
      *                  *---------------------------------------------*
      *                  * Lettura tabella causali di magazzino        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      l-mag-300-cod-cau    to   rf-zmc-cod-cau         .
           move      "pgm/mag/fls/ioc/obj/iofzmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zmc                 .
      *                  *---------------------------------------------*
      *                  * Se esito lettura OK : oltre                 *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to tsi-100.
      *                      *-----------------------------------------*
      *                      * Composizione messaggio                  *
      *                      *-----------------------------------------*
           move      "Causale di magazzino non esistente in archivio !  
      -              "                          "
                                          to   l-mag-300-msg-exi      .
      *                      *-----------------------------------------*
      *                      * Segnale di errore                       *
      *                      *-----------------------------------------*
           move      "#"                  to   l-mag-300-exi-sts      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     tsi-999.
       tsi-100.
      *                  *---------------------------------------------*
      *                  * Controllo trattamento valore per la causale *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se si e' in periodo di inventario di    *
      *                      * fine anno                               *
      *                      *-----------------------------------------*
           if        w-dnu-ife-ife-flg    =    spaces
                     go to tsi-140.
      *                          *-------------------------------------*
      *                          * Se data registrazione inferiore o   *
      *                          * pari alla data inventario           *
      *                          *-------------------------------------*
           if        l-mag-300-dat-reg    >    w-dnu-ife-ife-dti
                     go to tsi-130.
      *                              *---------------------------------*
      *                              * Ammesse causali con trattamento *
      *                              * valore come costi aggiuntivi o  *
      *                              * di rettifica costi              *
      *                              *---------------------------------*
           if        rf-zmc-trt-val       =    "C" or
                     rf-zmc-trt-val       =    "R"
                     go to tsi-200.
      *                                  *-----------------------------*
      *                                  * A trattamento errore        *
      *                                  *-----------------------------*
           go to     tsi-180.
       tsi-130.
      *                          *-------------------------------------*
      *                          * Se data registrazione superiore     *
      *                          * alla data inventario                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Ammessi tutti i tipi trattamen- *
      *                              * to valore per le causali        *
      *                              *---------------------------------*
           go to     tsi-200.
       tsi-140.
      *                      *-----------------------------------------*
      *                      * Se non si e' in periodo di inventario   *
      *                      * di fine anno                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ammessi tutti i tipi trattamento    *
      *                          * valore per le causali tranne che    *
      *                          * quelli relativi ai costi aggiunti-  *
      *                          * vi o rettifiche costo di fine eser- *
      *                          * cizio                               *
      *                          *-------------------------------------*
           if        rf-zmc-trt-val       not  = "X" and
                     rf-zmc-trt-val       not  = "Y"
                     go to tsi-200.
      *                              *---------------------------------*
      *                              * A trattamento errore            *
      *                              *---------------------------------*
           go to     tsi-180.
       tsi-180.
      *                  *---------------------------------------------*
      *                  * Trattamento errore se esito controllo nega- *
      *                  * tivo                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo trattamento valore per la causale non ammesso
      -              "               "    to   l-mag-300-msg-exi      .
      *                      *-----------------------------------------*
      *                      * Segnale di errore                       *
      *                      *-----------------------------------------*
           move      "#"                  to   l-mag-300-exi-sts      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     tsi-999.
       tsi-200.
      *              *-------------------------------------------------*
      *              * Uscita con flag a spazi                         *
      *              *-------------------------------------------------*
           move      spaces               to   l-mag-300-exi-sts      .
       tsi-999.
           exit.

      *    *===========================================================*
      *    * Test se movimento di 'MAG' modificabile o annullabile     *
      *    *-----------------------------------------------------------*
       tsm-000.
      *              *-------------------------------------------------*
      *              * Test su data registrazione minima               *
      *              *-------------------------------------------------*
           perform   ddm-000              thru ddm-999                .
           if        l-mag-300-dat-reg    <    w-nol-dtr-min
                     move   "#"           to   l-mag-300-exi-sts
                     go to  tsm-999.
       tsm-360.
      *              *-------------------------------------------------*
      *              * Controllo compatibilita' tra data registrazione *
      *              * e dati inventario di fine esercizio             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se si e' in periodo di inventario di fine   *
      *                  * anno                                        *
      *                  *---------------------------------------------*
           if        w-dnu-ife-ife-flg    =    spaces
                     go to tsm-370.
      *                      *-----------------------------------------*
      *                      * Se data registrazione inferiore o pari  *
      *                      * alla data inventario                    *
      *                      *-----------------------------------------*
           if        l-mag-300-dat-reg    >    w-dnu-ife-ife-dti
                     go to tsm-365.
      *                          *-------------------------------------*
      *                          * Modificabilita' della registrazione *
      *                          * solo per il valore                  *
      *                          *-------------------------------------*
           move      "V"                  to   l-mag-300-exi-sts      .
           go to     tsm-380.
       tsm-365.
      *                      *-----------------------------------------*
      *                      * Se data registrazione superiore alla    *
      *                      * data inventario                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Modificabilita' della registrazione *
      *                          * interamente                         *
      *                          *-------------------------------------*
           move      spaces               to   l-mag-300-exi-sts      .
           go to     tsm-380.
       tsm-370.
      *                  *---------------------------------------------*
      *                  * Se non si e' in periodo di inventario di    *
      *                  * fine anno                                   *
      *                  *---------------------------------------------*
      *                          *-------------------------------------*
      *                          * Modificabilita' della registrazione *
      *                          * interamente                         *
      *                          *-------------------------------------*
           move      spaces               to   l-mag-300-exi-sts      .
           go to     tsm-380.
       tsm-380.
      *              *-------------------------------------------------*
      *              * Uscita con flag a spazi                         *
      *              *-------------------------------------------------*
           move      spaces               to   l-mag-300-exi-sts      .
       tsm-999.
           exit.

      *    *===========================================================*
      *    * Test se movimento di 'MAG' esistente                      *
      *    *-----------------------------------------------------------*
       tse-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag                            *
      *              *-------------------------------------------------*
           move      spaces               to   l-mag-300-exi-sts      .
      *              *-------------------------------------------------*
      *              * Test su data registrazione e protocollo         *
      *              *-------------------------------------------------*
           if        l-mag-300-dat-reg    =    zero or
                     l-mag-300-num-prt    =    zero
                     go to tse-999.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      l-mag-300-dat-reg    to   rf-mmt-dat-reg         .
           move      l-mag-300-num-prt    to   rf-mmt-num-prt         .
           move      "pgm/mag/fls/ioc/obj/iofmmt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmt                 .
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to tse-999.
      *              *-------------------------------------------------*
      *              * Uscita con flag di movimento non trovato        *
      *              *-------------------------------------------------*
           move      "#"                  to   l-mag-300-exi-sts      .
       tse-999.
           exit.

      *    *===========================================================*
      *    * Attribuzione numero protocollo di mag                     *
      *    *-----------------------------------------------------------*
       rnp-000.
      *              *-------------------------------------------------*
      *              * Attribuzione numero protocollo di magazzino     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione secolo ed anno                 *
      *                  *---------------------------------------------*
           move      l-mag-300-dat-reg    to   s-dat                  .
           move      s-saa                to   w-prt-mag-ann-ese      .
      *                  *---------------------------------------------*
      *                  * Preparazione codice dipendenza              *
      *                  *---------------------------------------------*
           move      l-mag-300-cod-dpz    to   w-prt-mag-cod-dpz      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine di attribuzione            *
      *                  *---------------------------------------------*
           perform   att-prt-mag-000      thru att-prt-mag-999        .
      *                  *---------------------------------------------*
      *                  * Numero protocollo completo, in formato      *
      *                  * s.aa.dd.nnnnnn, in campo di destinazione    *
      *                  *---------------------------------------------*
           move      w-prt-mag-num-prt    to   l-mag-300-num-prt      .
       rnp-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento di mag                                   *
      *    *-----------------------------------------------------------*
       del-000.
      *              *-------------------------------------------------*
      *              * Cancellazione righe                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [mmr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATREG"             to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      l-mag-300-dat-reg    to   rf-mmr-dat-reg         .
           move      l-mag-300-num-prt    to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                      *-----------------------------------------*
      *                      * Se start non valida : a cancellazione   *
      *                      * testata                                 *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to del-500.
       del-100.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [mmr]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                      *-----------------------------------------*
      *                      * Se 'at end' : a cancellazione testata   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  del-500.
      *                      *-----------------------------------------*
      *                      * Test se oltre il massimo                *
      *                      *-----------------------------------------*
           if        rf-mmr-dat-reg       not  = l-mag-300-dat-reg or
                     rf-mmr-num-prt       not  = l-mag-300-num-prt
                     go to  del-500.
      *                  *---------------------------------------------*
      *                  * Delete record file [mmr]                    *
      *                  *---------------------------------------------*
           perform   del-rec-mmr-000      thru del-rec-mmr-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura riga corpo successiva     *
      *                  *---------------------------------------------*
           go to     del-100.
       del-500.
      *              *-------------------------------------------------*
      *              * Cancellazione testata file [mmt]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete record [mmt]                         *
      *                  *---------------------------------------------*
           perform   del-rec-mmt-000      thru del-rec-mmt-999        .
       del-999.
           exit.

      *    *===========================================================*
      *    * Cambio dislocazione prodotto                              *
      *    *                                                           *
      *    * N.B.: movimento semplificato con alcune forzature         *
      *    *-----------------------------------------------------------*
       cdp-000.
      *              *-------------------------------------------------*
      *              * Forzatura codice dipendenza                     *
      *              *-------------------------------------------------*
           move      01                   to   l-mag-300-cod-dpz      .
       cdp-100.
      *              *-------------------------------------------------*
      *              * Test se protocollo in input                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        l-mag-300-num-prt    not  = zero
                     go to cdp-200.
      *                  *---------------------------------------------*
      *                  * Attribuzione numero protocollo              *
      *                  *---------------------------------------------*
           perform   rnp-000              thru rnp-999                .
       cdp-200.
      *              *-------------------------------------------------*
      *              * Scrittura testata documento                     *
      *              *-------------------------------------------------*
           perform   wte-000              thru wte-999                .
       cdp-400.
      *              *-------------------------------------------------*
      *              * Forzatura tipo magazzino: prodotti di vendita   *
      *              *-------------------------------------------------*
           move      01                   to   l-mag-300-tip-mag      .
      *              *-------------------------------------------------*
      *              * Scrittura riga movimento di magazzino           *
      *              *-------------------------------------------------*
           perform   wrg-000              thru wrg-999                .
       cdp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cdp-999.
       cdp-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione numero protocollo MAG             *
      *    *-----------------------------------------------------------*
       att-prt-mag-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/inprtmag"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-mag             .
      *              *-------------------------------------------------*
      *              * Preparazione chiave per lettura tabella numera- *
      *              * zioni [prtmag]                                  *
      *              *-------------------------------------------------*
           move      w-prt-mag-ann-ese    to   rn-prt-mag-ann-ese     .
           move      w-prt-mag-cod-dpz    to   rn-prt-mag-cod-dpz     .
      *              *-------------------------------------------------*
      *              * Lettura tabella numerazioni [prtmag]            *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/inprtmag"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-mag             .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-prt-mag-400.
       att-prt-mag-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/inprtmag"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-mag             .
      *                  *---------------------------------------------*
      *                  * Unlock record                               *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/inprtmag"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-mag             .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'intera operazione          *
      *                  *---------------------------------------------*
           go to     att-prt-mag-000.
       att-prt-mag-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore pre-incremento           *
      *                  *---------------------------------------------*
           move      rn-prt-mag-num-prt   to   w-prt-mag-num-prt      .
           move      w-prt-mag-r01-num    to   w-prt-mag-val-pre      .
      *                  *---------------------------------------------*
      *                  * Preparazione valore post-incremento         *
      *                  *---------------------------------------------*
           move      w-prt-mag-val-pre    to   w-prt-mag-val-pos      .
           add       1                    to   w-prt-mag-val-pos      .
           if        w-prt-mag-val-pos    =    zero
                     move  1              to   w-prt-mag-val-pos      .
      *                  *---------------------------------------------*
      *                  * Preparazione numero protocollo completo     *
      *                  *---------------------------------------------*
           move      w-prt-mag-ann-ese    to   w-prt-mag-r01-ese      .
           move      w-prt-mag-cod-dpz    to   w-prt-mag-r01-dpz      .
           move      w-prt-mag-val-pos    to   w-prt-mag-r01-num      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento numero protocollo             *
      *                  *---------------------------------------------*
           move      w-prt-mag-num-prt    to   rn-prt-mag-num-prt     .
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/inprtmag"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-mag             .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito dell'opera- *
      *                  * zione di update                             *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-prt-mag-600.
       att-prt-mag-500.
      *                  *---------------------------------------------*
      *                  * Se errore in operazione di update           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock record                           *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/inprtmag"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-mag             .
      *                      *-----------------------------------------*
      *                      * Ripetizione dell'intera operazione      *
      *                      *-----------------------------------------*
           go to     att-prt-mag-000.
       att-prt-mag-600.
      *                  *---------------------------------------------*
      *                  * Se nessun errore in operazione di update    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock record                           *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/inprtmag"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-mag             .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     att-prt-mag-999.
       att-prt-mag-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [mmr]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-mmr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      l-mag-300-dat-reg    to   rf-mmr-dat-reg         .
           move      l-mag-300-num-prt    to   rf-mmr-num-prt         .
           move      w-wrk-ctr-rig        to   rf-mmr-num-prg         .
           move      l-mag-300-cod-dpz    to   rf-mmr-cod-dpz         .
           move      l-mag-300-cod-cau    to   rf-mmr-cod-cau         .
           move      w-zmc-trt-val        to   rf-mmr-trt-val         .
           move      w-zmc-tip-mdm        to   rf-mmr-tip-mdm         .
           move      w-zmc-trt-mic        to   rf-mmr-trt-mic         .
           move      w-zmc-tip-mic        to   rf-mmr-tip-mic         .
           move      l-mag-300-cod-ctm    to   rf-mmr-cod-ctm         .
           move      l-mag-300-dat-doc    to   rf-mmr-dat-doc         .
           move      l-mag-300-num-doc    to   rf-mmr-num-doc         .
           move      l-mag-300-cod-dsl    to   rf-mmr-cod-dsl         .
           move      l-mag-300-cod-dsd    to   rf-mmr-cod-dsd         .
           move      l-mag-300-tip-arc    to   rf-mmr-tip-arc         .
           move      l-mag-300-cod-arc    to   rf-mmr-cod-arc         .
           move      l-mag-300-dpz-arc    to   rf-mmr-dpz-arc         .
           move      l-mag-300-tip-mag    to   rf-mmr-tip-mag         .
           move      l-mag-300-num-mag    to   rf-mmr-num-mag         .
           move      l-mag-300-alf-mag    to   rf-mmr-alf-mag         .
           move      l-mag-300-var-mag    to   rf-mmr-var-mag         .
           move      l-mag-300-udm-ana    to   rf-mmr-udm-ana         .
           move      l-mag-300-udm-mov    to   rf-mmr-udm-mov         .
           move      l-mag-300-num-pmg    to   rf-mmr-num-pmg         .
           move      l-mag-300-ddo-rif    to   rf-mmr-ddo-rif         .
           move      l-mag-300-ndo-rif    to   rf-mmr-ndo-rif         .
           move      l-mag-300-qta-mov    to   rf-mmr-qta-mov         .
           move      l-mag-300-cun-mov    to   rf-mmr-cun-mov         .
           move      l-mag-300-val-mov    to   rf-mmr-val-mov         .
       cmp-rec-mmr-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record righe [mmr]                              *
      *    *-----------------------------------------------------------*
       wrt-rec-mmr-000.
      *              *-------------------------------------------------*
      *              * Aggiustamento dei decimali                      *
      *              *                                                 *
      *              * Necessario in quanto il campo in input viene    *
      *              * gestito diversamente da quello in output        *
      *              *-------------------------------------------------*
           multiply  100                  by   l-mag-300-cun-mov      .
       wrt-rec-mmr-100.
      *              *-------------------------------------------------*
      *              * Composizione record [mmr]                       *
      *              *-------------------------------------------------*
           perform   cmp-rec-mmr-000      thru cmp-rec-mmr-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento [mms],[mmz] e [mmv] in piu'       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione area aggiornamento w-agg-mag   *
      *                  * per l'aggiornamento in piu'                 *
      *                  *---------------------------------------------*
           perform   paa-mag-piu-000      thru paa-mag-piu-999        .
      *                  *---------------------------------------------*
      *                  * Aggiornamento [mms], [mmz] e [mmv] secondo  *
      *                  * l'area w-agg-mag                            *
      *                  *---------------------------------------------*
           perform   agg-ges-mag-000      thru agg-ges-mag-999        .
      *              *-------------------------------------------------*
      *              * Put record [mmr]                                *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
       wrt-rec-mmr-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record righe [mmr]                          *
      *    *-----------------------------------------------------------*
       del-rec-mmr-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento [mms],[mmz] e [mmv] in meno       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione area aggiornamento w-agg-mag   *
      *                  * per l'aggiornamento in meno                 *
      *                  *---------------------------------------------*
           perform   paa-mag-men-000      thru paa-mag-men-999        .
      *                  *---------------------------------------------*
      *                  * Aggiornamento [mms], [mmz] e [mmv] secondo  *
      *                  * l'area w-agg-mag                            *
      *                  *---------------------------------------------*
           perform   agg-ges-mag-000      thru agg-ges-mag-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
       del-rec-mmr-999.
           exit.

      *    *===========================================================*
      *    * Composizione record testata [mmt]                         *
      *    *-----------------------------------------------------------*
       cmp-rec-mmt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmt                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-mmt-ide-dat         .
           move      s-ute                to   rf-mmt-ide-ute         .
           move      s-fas                to   rf-mmt-ide-fas         .
           move      l-mag-300-dat-reg    to   rf-mmt-dat-reg         .
           move      l-mag-300-num-prt    to   rf-mmt-num-prt         .
           move      l-mag-300-cod-dpz    to   rf-mmt-cod-dpz         .
           move      l-mag-300-cod-cau    to   rf-mmt-cod-cau         .
           move      w-zmc-trt-val        to   rf-mmt-trt-val         .
           move      w-zmc-tip-mdm        to   rf-mmt-tip-mdm         .
           move      w-zmc-trt-mic        to   rf-mmt-trt-mic         .
           move      w-zmc-tip-mic        to   rf-mmt-tip-mic         .
           move      w-zmc-cod-mic        to   rf-mmt-cod-mic         .
           move      w-zmc-snv-mic        to   rf-mmt-snv-mic         .
           move      w-zmc-def-tar        to   rf-mmt-def-tar         .
           move      w-zmc-snv-tar        to   rf-mmt-snv-tar         .
           move      w-zmc-lst-tar        to   rf-mmt-lst-tar         .
           move      spaces               to   rf-mmt-def-dsl         .
           move      spaces               to   rf-mmt-def-dsd         .
           move      spaces               to   rf-mmt-snv-dsl         .
           move      l-mag-300-cod-ctm    to   rf-mmt-cod-ctm         .
           move      l-mag-300-dat-doc    to   rf-mmt-dat-doc         .
           move      l-mag-300-num-doc    to   rf-mmt-num-doc         .
           move      l-mag-300-cod-dsl    to   rf-mmt-cod-dsl         .
           move      l-mag-300-cod-dsd    to   rf-mmt-cod-dsd         .
           move      l-mag-300-tip-arc    to   rf-mmt-tip-arc         .
           move      l-mag-300-cod-arc    to   rf-mmt-cod-arc         .
           move      l-mag-300-dpz-arc    to   rf-mmt-dpz-arc         .
       cmp-rec-mmt-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record testata [mmt]                            *
      *    *-----------------------------------------------------------*
       wrt-rec-mmt-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mmt-000      thru cmp-rec-mmt-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmt                 .
       wrt-rec-mmt-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [mmt]                                *
      *    *-----------------------------------------------------------*
       del-rec-mmt-000.
      *              *-------------------------------------------------*
      *              * Composizione chiave primaria                    *
      *              *-------------------------------------------------*
           move      l-mag-300-dat-reg    to   rf-mmt-dat-reg         .
           move      l-mag-300-num-prt    to   rf-mmt-num-prt         .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmt                 .
       del-rec-mmt-999.
           exit.

      *    *===========================================================*
      *    * Preparazione area di aggiornamento w-agg-mag per aggior-  *
      *    * namento in piu'                                           *
      *    *-----------------------------------------------------------*
       paa-mag-piu-000.
      *              *-------------------------------------------------*
      *              * Tipo di aggiornamento                           *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-mag-tip-agg      .
      *              *-------------------------------------------------*
      *              * Data di registrazione del movimento             *
      *              *-------------------------------------------------*
           move      l-mag-300-dat-reg    to   w-agg-mag-dat-reg      .
      *              *-------------------------------------------------*
      *              * Esercizio per inventario di fine anno           *
      *              *-------------------------------------------------*
           if        w-dnu-ife-ife-flg    not  = spaces
                     move  w-dnu-ife-ife-ese
                                          to   w-agg-mag-ese-inv
           else      move  zero           to   w-agg-mag-ese-inv      .
      *              *-------------------------------------------------*
      *              * Trattamento valore per il movimento             *
      *              *-------------------------------------------------*
           move      w-zmc-trt-val        to   w-agg-mag-trt-val      .
      *              *-------------------------------------------------*
      *              * Tipo movimentazione di magazzino                *
      *              *-------------------------------------------------*
           move      w-zmc-tip-mdm        to   w-agg-mag-tip-mdm      .
      *              *-------------------------------------------------*
      *              * Tipo trattamento conto merce                    *
      *              *-------------------------------------------------*
           move      w-zmc-trt-mic        to   w-agg-mag-trt-mic      .
      *              *-------------------------------------------------*
      *              * Tipo conto merce                                *
      *              *-------------------------------------------------*
           move      w-zmc-tip-mic        to   w-agg-mag-tip-mic      .
      *              *-------------------------------------------------*
      *              * Codice conto merce                              *
      *              *-------------------------------------------------*
           move      l-mag-300-cod-ctm    to   w-agg-mag-cod-mic      .
      *              *-------------------------------------------------*
      *              * Tipo archivio                                   *
      *              *-------------------------------------------------*
           move      l-mag-300-tip-arc    to   w-agg-mag-tip-arc      .
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           move      l-mag-300-cod-arc    to   w-agg-mag-cod-arc      .
      *              *-------------------------------------------------*
      *              * Codice dipendenza per il codice archivio        *
      *              *-------------------------------------------------*
           move      l-mag-300-dpz-arc    to   w-agg-mag-dpz-arc      .
      *              *-------------------------------------------------*
      *              * Codice dipendenza di riferimento                *
      *              *-------------------------------------------------*
           move      l-mag-300-cod-dpz    to   w-agg-mag-cod-dpz      .
      *              *-------------------------------------------------*
      *              * Tipo codice di magazzino                        *
      *              *-------------------------------------------------*
           move      l-mag-300-tip-mag    to   w-agg-mag-tip-mag      .
      *              *-------------------------------------------------*
      *              * Codice numerico di magazzino                    *
      *              *-------------------------------------------------*
           move      l-mag-300-num-mag    to   w-agg-mag-num-mag      .
      *              *-------------------------------------------------*
      *              * Sigla della variante per il codice numerico di  *
      *              * magazzino                                       *
      *              *-------------------------------------------------*
           move      l-mag-300-var-mag    to   w-agg-mag-var-mag      .
      *              *-------------------------------------------------*
      *              * Codice dislocazione                             *
      *              *-------------------------------------------------*
           move      l-mag-300-cod-dsl    to   w-agg-mag-cod-dsl      .
      *              *-------------------------------------------------*
      *              * Codice dislocazione di destinazione             *
      *              *-------------------------------------------------*
           move      l-mag-300-cod-dsd    to   w-agg-mag-cod-dsd      .
      *              *-------------------------------------------------*
      *              * Codice unita' di misura su anagrafica magazzino *
      *              *-------------------------------------------------*
           move      l-mag-300-udm-ana    to   w-agg-mag-udm-ana      .
      *              *-------------------------------------------------*
      *              * Codice unita' di misura per il movimento        *
      *              *-------------------------------------------------*
           move      l-mag-300-udm-mov    to   w-agg-mag-udm-mov      .
      *              *-------------------------------------------------*
      *              * Quantita' del movimento                         *
      *              *-------------------------------------------------*
           move      l-mag-300-qta-mov    to   w-agg-mag-qta-mov      .
      *              *-------------------------------------------------*
      *              * Costo unitario del movimento                    *
      *              *-------------------------------------------------*
           move      l-mag-300-cun-mov    to   w-agg-mag-cun-mov      .
      *              *-------------------------------------------------*
      *              * Valore del movimento                            *
      *              *-------------------------------------------------*
           move      l-mag-300-val-mov    to   w-agg-mag-val-mov      .
       paa-mag-piu-999.
           exit.

      *    *===========================================================*
      *    * Preparazione area di aggiornamento w-agg-mag per aggior-  *
      *    * namento in meno                                           *
      *    *-----------------------------------------------------------*
       paa-mag-men-000.
      *              *-------------------------------------------------*
      *              * Tipo di aggiornamento                           *
      *              *-------------------------------------------------*
           move      "-"                  to   w-agg-mag-tip-agg      .
      *              *-------------------------------------------------*
      *              * Data di registrazione del movimento             *
      *              *-------------------------------------------------*
           move      l-mag-300-dat-reg    to   w-agg-mag-dat-reg      .
      *              *-------------------------------------------------*
      *              * Esercizio per inventario di fine anno           *
      *              *-------------------------------------------------*
           if        w-dnu-ife-ife-flg    not  = spaces
                     move  w-dnu-ife-ife-ese
                                          to   w-agg-mag-ese-inv
           else      move  zero           to   w-agg-mag-ese-inv      .
      *              *-------------------------------------------------*
      *              * Trattamento valore per il movimento             *
      *              *-------------------------------------------------*
           move      rf-mmr-trt-val       to   w-agg-mag-trt-val      .
      *              *-------------------------------------------------*
      *              * Tipo movimentazione di magazzino                *
      *              *-------------------------------------------------*
           move      rf-mmr-tip-mdm       to   w-agg-mag-tip-mdm      .
      *              *-------------------------------------------------*
      *              * Tipo trattamento conto merce                    *
      *              *-------------------------------------------------*
           move      rf-mmr-trt-mic       to   w-agg-mag-trt-mic      .
      *              *-------------------------------------------------*
      *              * Tipo conto merce                                *
      *              *-------------------------------------------------*
           move      rf-mmr-tip-mic       to   w-agg-mag-tip-mic      .
      *              *-------------------------------------------------*
      *              * Codice conto merce                              *
      *              *-------------------------------------------------*
           move      rf-mmr-cod-ctm       to   w-agg-mag-cod-mic      .
      *              *-------------------------------------------------*
      *              * Tipo archivio                                   *
      *              *-------------------------------------------------*
           move      rf-mmr-tip-arc       to   w-agg-mag-tip-arc      .
      *              *-------------------------------------------------*
      *              * Codice archivio                                 *
      *              *-------------------------------------------------*
           move      rf-mmr-cod-arc       to   w-agg-mag-cod-arc      .
      *              *-------------------------------------------------*
      *              * Codice dipendenza per il codice archivio        *
      *              *-------------------------------------------------*
           move      rf-mmr-dpz-arc       to   w-agg-mag-dpz-arc      .
      *              *-------------------------------------------------*
      *              * Codice dipendenza di riferimento                *
      *              *-------------------------------------------------*
           move      rf-mmr-cod-dpz       to   w-agg-mag-cod-dpz      .
      *              *-------------------------------------------------*
      *              * Tipo codice di magazzino                        *
      *              *-------------------------------------------------*
           move      rf-mmr-tip-mag       to   w-agg-mag-tip-mag      .
      *              *-------------------------------------------------*
      *              * Codice numerico di magazzino                    *
      *              *-------------------------------------------------*
           move      rf-mmr-num-mag       to   w-agg-mag-num-mag      .
      *              *-------------------------------------------------*
      *              * Sigla della variante per il codice numerico di  *
      *              * magazzino                                       *
      *              *-------------------------------------------------*
           move      rf-mmr-var-mag       to   w-agg-mag-var-mag      .
      *              *-------------------------------------------------*
      *              * Codice dislocazione                             *
      *              *-------------------------------------------------*
           move      rf-mmr-cod-dsl       to   w-agg-mag-cod-dsl      .
      *              *-------------------------------------------------*
      *              * Codice dislocazione di destinazione             *
      *              *-------------------------------------------------*
           move      rf-mmr-cod-dsd       to   w-agg-mag-cod-dsd      .
      *              *-------------------------------------------------*
      *              * Codice unita' di misura su anagrafica magazzino *
      *              *-------------------------------------------------*
           move      rf-mmr-udm-ana       to   w-agg-mag-udm-ana      .
      *              *-------------------------------------------------*
      *              * Codice unita' di misura per il movimento        *
      *              *-------------------------------------------------*
           move      rf-mmr-udm-mov       to   w-agg-mag-udm-mov      .
      *              *-------------------------------------------------*
      *              * Quantita' del movimento                         *
      *              *-------------------------------------------------*
           move      rf-mmr-qta-mov       to   w-agg-mag-qta-mov      .
      *              *-------------------------------------------------*
      *              * Costo unitario del movimento                    *
      *              *-------------------------------------------------*
           move      rf-mmr-cun-mov       to   w-agg-mag-cun-mov      .
      *              *-------------------------------------------------*
      *              * Valore del movimento                            *
      *              *-------------------------------------------------*
           move      rf-mmr-val-mov       to   w-agg-mag-val-mov      .
       paa-mag-men-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamenti per gestione magazzino MAG                  *
      *    *-----------------------------------------------------------*
       agg-ges-mag-000.
      *              *-------------------------------------------------*
      *              * Se non ancora fatto, si determina il mese di    *
      *              * chiusura esercizio per la gestione magazzino,   *
      *              * mutuandolo dalla contabilita' generale          *
      *              *-------------------------------------------------*
           if        w-agg-mag-mes-che    not  = zero
                     go to agg-ges-mag-010.
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[mes-chi]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-agg-mag-mes-che
           else      move  12             to   w-agg-mag-mes-che      .
           if        w-agg-mag-mes-che    <    01 or
                     w-agg-mag-mes-che    >    12
                     move  12             to   w-agg-mag-mes-che      .
       agg-ges-mag-010.
      *              *-------------------------------------------------*
      *              * Determinazione anno di esercizio in funzione    *
      *              * della data di registrazione passata come pa-    *
      *              * rametro                                         *
      *              *-------------------------------------------------*
           move      w-agg-mag-dat-reg    to   s-dat                  .
           if        w-agg-mag-mes-che    =    12             or
                     s-mes                >    w-agg-mag-mes-che
                     move  s-saa          to   w-agg-mag-ann-ese
                     go to agg-ges-mag-020.
           subtract  1                    from s-saa                  .
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-agg-mag-ann-ese      .
       agg-ges-mag-020.
      *              *-------------------------------------------------*
      *              * Determinazione mese di esercizio in funzione    *
      *              * della data di registrazione passata come pa-    *
      *              * rametro                                         *
      *              *-------------------------------------------------*
           move      s-mes                to   w-agg-mag-mes-ese      .
           if        w-agg-mag-mes-che    =    12
                     go to agg-ges-mag-050.
           if        s-mes                >    w-agg-mag-mes-che
                     subtract w-agg-mag-mes-che
                                          from w-agg-mag-mes-ese
                     go to agg-ges-mag-050.
           if        s-mes                <    w-agg-mag-mes-che
                     add   12             to   w-agg-mag-mes-ese
                     subtract w-agg-mag-mes-che
                                          from w-agg-mag-mes-ese
                     go to agg-ges-mag-050.
           move      12                   to   w-agg-mag-mes-ese      .
       agg-ges-mag-050.
      *              *-------------------------------------------------*
      *              * Aggiornamento [mms]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il tipo codice di magazzino non e' tra   *
      *                  * quelli previsti : uscita senza alcun ag-    *
      *                  * giornamento                                 *
      *                  *---------------------------------------------*
           if        w-agg-mag-tip-mag    not  = 01 and
                     w-agg-mag-tip-mag    not  = 02 and
                     w-agg-mag-tip-mag    not  = 03 and
                     w-agg-mag-tip-mag    not  = 04
                     go to agg-ges-mag-999.
      *                  *---------------------------------------------*
      *                  * Se manca il codice numerico di magazzino :  *
      *                  * uscita senza alcun aggiornamento            *
      *                  *---------------------------------------------*
           if        w-agg-mag-num-mag    =    zero
                     go to agg-ges-mag-999.
      *                  *---------------------------------------------*
      *                  * Se quantita' del movimento pari a zero :    *
      *                  * ad aggiornamento [mmv]                      *
      *                  *---------------------------------------------*
           if        w-agg-mag-qta-mov    =    zero
                     go to agg-ges-mag-400.
      *                  *---------------------------------------------*
      *                  * Se il tipo trattamento conto merce e' 01,   *
      *                  * cioe' il movimento puo' riferirsi ad un al- *
      *                  * tro movimento di merce in conto, ed inoltre *
      *                  * e' citato un codice di merce in conto, al-  *
      *                  * lora non si esegue alcun aggiornamento re-  *
      *                  * lativamente a [mms], in quanto l'aggiorna-  *
      *                  * mento riguardera' esclusivamente [mmz]      *
      *                  *---------------------------------------------*
           if        w-agg-mag-trt-mic    =    01   and
                     w-agg-mag-cod-mic    not  = spaces
                     go to agg-ges-mag-300.
       agg-ges-mag-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento relativo alla prima disloca-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [mms]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [mms] con lock           *
      *                      *-----------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "DPMGDS    "         to   f-key                  .
           move      w-agg-mag-ann-ese    to   rf-mms-ann-ese         .
           move      w-agg-mag-cod-dpz    to   rf-mms-cod-dpz         .
           move      w-agg-mag-tip-mag    to   rf-mms-tip-mag         .
           move      w-agg-mag-num-mag    to   rf-mms-num-mag         .
           move      w-agg-mag-var-mag    to   rf-mms-var-mag         .
           move      w-agg-mag-cod-dsl    to   rf-mms-cod-dsl         .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to agg-ges-mag-110.
      *                      *-----------------------------------------*
      *                      * Se record non esistente                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Scrittura del record normalizzato   *
      *                          *-------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                          *-------------------------------------*
      *                          * Unlock del record                   *
      *                          *-------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                          *-------------------------------------*
      *                          * Riciclo a lettura con lock          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-100.
       agg-ges-mag-110.
      *                      *-----------------------------------------*
      *                      * Se record esistente                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se si deve ag- *
      *                          * giornare il carico o lo scarico     *
      *                          *-------------------------------------*
           if        w-agg-mag-tip-mdm    =    01
                     go to agg-ges-mag-120
           else if   w-agg-mag-tip-mdm    =    02 or
                     w-agg-mag-tip-mdm    =    03
                     go to agg-ges-mag-130.
       agg-ges-mag-120.
      *                          *-------------------------------------*
      *                          * Aggiornamento del carico, in piu' o *
      *                          * in meno a seconda del tipo di ag-   *
      *                          * giornamento                         *
      *                          *-------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     subtract  w-agg-mag-qta-mov
                                          from rf-mms-prg-car
                                              (w-agg-mag-mes-ese)
           else      add       w-agg-mag-qta-mov
                                          to   rf-mms-prg-car
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-140.
       agg-ges-mag-130.
      *                          *-------------------------------------*
      *                          * Aggiornamento dello scarico, in piu'*
      *                          * o in meno a seconda del tipo di ag- *
      *                          * giornamento                         *
      *                          *-------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     subtract  w-agg-mag-qta-mov
                                          from rf-mms-prg-sca
                                              (w-agg-mag-mes-ese)
           else      add       w-agg-mag-qta-mov
                                          to   rf-mms-prg-sca
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-140.
       agg-ges-mag-140.
      *                      *-----------------------------------------*
      *                      * Update record [mms]                     *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * operazione di update                    *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to agg-ges-mag-150.
      *                      *-----------------------------------------*
      *                      * Se errori in update                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Unlock [mms]                        *
      *                          *-------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                          *-------------------------------------*
      *                          * Ripetizione operazione di aggior-   *
      *                          * namento relativa alla prima dislo-  *
      *                          * cazione                             *
      *                          *-------------------------------------*
           go to     agg-ges-mag-100.
       agg-ges-mag-150.
      *                      *-----------------------------------------*
      *                      * Se nessun errore in update              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Unlock [mms]                        *
      *                          *-------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                  *---------------------------------------------*
      *                  * Test se deve essere eseguito l'aggiornamen- *
      *                  * to relativo alla seconda dislocazione       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo movimentazione di magazzino  *
      *                      * non e' di tipo 03, ovvero di movimenta- *
      *                      * zione interna, non si esegue l'aggior-  *
      *                      * namento relativo alla seconda disloca-  *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        w-agg-mag-tip-mdm    not  = 03
                     go to agg-ges-mag-300.
       agg-ges-mag-200.
      *                  *---------------------------------------------*
      *                  * Aggiornamento relativo alla seconda dislo-  *
      *                  * cazione                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [mms]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [mms] con lock           *
      *                      *-----------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "DPMGDS    "         to   f-key                  .
           move      w-agg-mag-ann-ese    to   rf-mms-ann-ese         .
           move      w-agg-mag-cod-dpz    to   rf-mms-cod-dpz         .
           move      w-agg-mag-tip-mag    to   rf-mms-tip-mag         .
           move      w-agg-mag-num-mag    to   rf-mms-num-mag         .
           move      w-agg-mag-var-mag    to   rf-mms-var-mag         .
           move      w-agg-mag-cod-dsd    to   rf-mms-cod-dsl         .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to agg-ges-mag-210.
      *                      *-----------------------------------------*
      *                      * Se record non esistente                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Scrittura del record normalizzato   *
      *                          *-------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                          *-------------------------------------*
      *                          * Unlock del record                   *
      *                          *-------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                          *-------------------------------------*
      *                          * Riciclo a lettura con lock          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-200.
       agg-ges-mag-210.
      *                      *-----------------------------------------*
      *                      * Se record esistente                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento del carico, in piu' o *
      *                          * in meno a seconda del tipo di ag-   *
      *                          * giornamento                         *
      *                          *-------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     subtract  w-agg-mag-qta-mov
                                          from rf-mms-prg-car
                                              (w-agg-mag-mes-ese)
           else      add       w-agg-mag-qta-mov
                                          to   rf-mms-prg-car
                                              (w-agg-mag-mes-ese)     .
      *                      *-----------------------------------------*
      *                      * Update record [mms]                     *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * operazione di update                    *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to agg-ges-mag-250.
      *                      *-----------------------------------------*
      *                      * Se errori in update                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Unlock [mms]                        *
      *                          *-------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                          *-------------------------------------*
      *                          * Ripetizione operazione di aggior-   *
      *                          * namento relativa alla seconda di-   *
      *                          * slocazione                          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-200.
       agg-ges-mag-250.
      *                      *-----------------------------------------*
      *                      * Se nessun errore in update              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Unlock [mms]                        *
      *                          *-------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
       agg-ges-mag-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento [mmz]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il tipo trattamento conto merce e' 03,   *
      *                  * cioe' il movimento stesso e' di movimenta-  *
      *                  * zione di merce in conto, si esegue l'ag-    *
      *                  * giornamento, purche' esistano e siano cor-  *
      *                  * retti gli altri parametri di aggiornamento  *
      *                  *---------------------------------------------*
           if        w-agg-mag-trt-mic    =    03
                     go to agg-ges-mag-305.
      *                  *---------------------------------------------*
      *                  * Se il tipo trattamento conto merce e' 02,   *
      *                  * cioe' il movimento non puo' riferirsi ad    *
      *                  * un altro movimento di merce in conto, non   *
      *                  * si esegue alcun aggiornamento di [mmz]      *
      *                  *---------------------------------------------*
           if        w-agg-mag-trt-mic    =    02
                     go to agg-ges-mag-400.
      *                  *---------------------------------------------*
      *                  * Se il tipo trattamento conto merce e' 01,   *
      *                  * cioe' il movimento puo' riferirsi ad un al- *
      *                  * tro movimento di merce in conto, ma non e'  *
      *                  * citato alcun codice di merce in conto, al-  *
      *                  * lora non si esegue alcun aggiornamento re-  *
      *                  * lativamente a [mmz], in quanto l'aggiorna-  *
      *                  * mento riguardava esclusivamente [mms]       *
      *                  *---------------------------------------------*
           if        w-agg-mag-cod-mic    =    spaces
                     go to agg-ges-mag-400.
       agg-ges-mag-305.
      *                  *---------------------------------------------*
      *                  * Se il tipo conto merce non e' tra quelli    *
      *                  * previsti : nessun aggiornamento             *
      *                  *---------------------------------------------*
           if        w-agg-mag-tip-mic    not  = "N" and
                     w-agg-mag-tip-mic    not  = "T"
                     go to agg-ges-mag-400.
      *                  *---------------------------------------------*
      *                  * Se manca il codice conto merce : nessun     *
      *                  * aggiornamento                               *
      *                  *---------------------------------------------*
           if        w-agg-mag-cod-mic    =    spaces
                     go to agg-ges-mag-400.
      *                  *---------------------------------------------*
      *                  * Se il tipo archivio non e' tra quelli       *
      *                  * previsti : nessun aggiornamento             *
      *                  *---------------------------------------------*
           if        w-agg-mag-tip-arc    not  = "C" and
                     w-agg-mag-tip-arc    not  = "F" and
                     w-agg-mag-tip-arc    not  = "A"
                     go to agg-ges-mag-400.
      *                  *---------------------------------------------*
      *                  * Se manca il codice archivio : nessun        *
      *                  * aggiornamento                               *
      *                  *---------------------------------------------*
           if        w-agg-mag-cod-arc    =    zero
                     go to agg-ges-mag-400.
       agg-ges-mag-310.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mmz]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [mmz] con lock               *
      *                  *---------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "MAGARC    "         to   f-key                  .
           move      w-agg-mag-ann-ese    to   rf-mmz-ann-ese         .
           move      w-agg-mag-cod-dpz    to   rf-mmz-cod-dpz         .
           move      w-agg-mag-tip-mag    to   rf-mmz-tip-mag         .
           move      w-agg-mag-num-mag    to   rf-mmz-num-mag         .
           move      w-agg-mag-var-mag    to   rf-mmz-var-mag         .
           move      w-agg-mag-tip-mic    to   rf-mmz-tip-ctm         .
           move      w-agg-mag-cod-mic    to   rf-mmz-cod-ctm         .
           move      w-agg-mag-tip-arc    to   rf-mmz-tip-arc         .
           move      w-agg-mag-cod-arc    to   rf-mmz-cod-arc         .
           move      w-agg-mag-dpz-arc    to   rf-mmz-dpz-arc         .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to agg-ges-mag-320.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Scrittura del record normalizzato       *
      *                      *-----------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                      *-----------------------------------------*
      *                      * Unlock del record                       *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura con lock              *
      *                      *-----------------------------------------*
           go to     agg-ges-mag-310.
       agg-ges-mag-320.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se si deve aggior- *
      *                      * nare il credito o il debito             *
      *                      *-----------------------------------------*
           if        w-agg-mag-trt-mic    =    03
                     go to agg-ges-mag-325.
           if        w-agg-mag-tip-mdm    =    02
                     go to agg-ges-mag-340
           else      go to agg-ges-mag-330.
       agg-ges-mag-325.
           if        w-agg-mag-tip-mdm    =    02
                     go to agg-ges-mag-330
           else      go to agg-ges-mag-340.
       agg-ges-mag-330.
      *                          *-------------------------------------*
      *                          * Aggiornamento del credito, in piu'  *
      *                          * o in meno a seconda del tipo di     *
      *                          * aggiornamento                       *
      *                          *-------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     subtract  w-agg-mag-qta-mov
                                          from rf-mmz-prg-cre
                                              (w-agg-mag-mes-ese)
           else      add       w-agg-mag-qta-mov
                                          to   rf-mmz-prg-cre
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-350.
       agg-ges-mag-340.
      *                          *-------------------------------------*
      *                          * Aggiornamento del debito, in piu'   *
      *                          * o in meno a seconda del tipo di     *
      *                          * aggiornamento                       *
      *                          *-------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     subtract  w-agg-mag-qta-mov
                                          from rf-mmz-prg-deb
                                              (w-agg-mag-mes-ese)
           else      add       w-agg-mag-qta-mov
                                          to   rf-mmz-prg-deb
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-350.
       agg-ges-mag-350.
      *                  *---------------------------------------------*
      *                  * Update record [mmz]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della ope-  *
      *                  * razione di update                           *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to agg-ges-mag-360.
      *                  *---------------------------------------------*
      *                  * Se errori in update                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [mmz]                            *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
      *                      *-----------------------------------------*
      *                      * Ripetizione operazione di aggiornamento *
      *                      * relativa a [mmz]                        *
      *                      *-----------------------------------------*
           go to     agg-ges-mag-310.
       agg-ges-mag-360.
      *                  *---------------------------------------------*
      *                  * Se nessun errore in update                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [mmz]                            *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmz"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmz                 .
       agg-ges-mag-400.
      *              *-------------------------------------------------*
      *              * Aggiornamento [mmv]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
       agg-ges-mag-404.
      *                      *-----------------------------------------*
      *                      * Test se attiva gestione dipendenze      *
      *                      * parallele                               *
      *                      *-----------------------------------------*
           if        w-prs-snx-gdp-snx    not  = "S"
                     go to agg-ges-mag-408.
      *                      *-----------------------------------------*
      *                      * Se la gestione dipendenze parallele e'  *
      *                      * attiva ed il movimento in corso di      *
      *                      * trattamento appartiene ad una di esse,  *
      *                      * il valore relativo viene ignorato       *
      *                      *-----------------------------------------*
           if        w-agg-mag-cod-dpz    >    50
                     go to agg-ges-mag-999.
       agg-ges-mag-408.
      *                      *-----------------------------------------*
      *                      * Se nessun trattamento valore : uscita   *
      *                      *-----------------------------------------*
           if        w-agg-mag-trt-val    =    "N"
                     go to agg-ges-mag-999.
      *                      *-----------------------------------------*
      *                      * Se trattamento valore per aggiustamento *
      *                      * costi di fine esercizio e esercizio per *
      *                      * inventario di fine anno a zero : uscita *
      *                      *-----------------------------------------*
           if       (w-agg-mag-trt-val    =    "X" or
                     w-agg-mag-trt-val    =    "Y"  ) and
                     w-agg-mag-ese-inv    =    zero
                     go to agg-ges-mag-999.
      *                      *-----------------------------------------*
      *                      * Se valore a zero : uscita, a meno che   *
      *                      * il tipo valorizzazione sia per imposta- *
      *                      * zione diretta o per costo standard      *
      *                      *-----------------------------------------*
           if        w-agg-mag-trt-val    =    "I" or
                     w-agg-mag-trt-val    =    "S"
                     go to agg-ges-mag-410.
           if        w-agg-mag-val-mov    =    zero
                     go to agg-ges-mag-999.
       agg-ges-mag-410.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mmv]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [mmv] con lock               *
      *                  *---------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           if        w-agg-mag-trt-val    =    "X" or
                     w-agg-mag-trt-val    =    "Y"
                     move  w-agg-mag-ese-inv
                                          to   rf-mmv-ann-ese
           else      move  w-agg-mag-ann-ese
                                          to   rf-mmv-ann-ese         .
           move      w-agg-mag-tip-mag    to   rf-mmv-tip-mag         .
           move      w-agg-mag-num-mag    to   rf-mmv-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to agg-ges-mag-420.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Scrittura del record normalizzato       *
      *                      *-----------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                      *-----------------------------------------*
      *                      * Unlock del record                       *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura con lock              *
      *                      *-----------------------------------------*
           go to     agg-ges-mag-410.
       agg-ges-mag-420.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del trattamento va-  *
      *                  * lore                                        *
      *                  *---------------------------------------------*
           if        w-agg-mag-trt-val    =    "I"
                     go to agg-ges-mag-440
           else if   w-agg-mag-trt-val    =    "S"
                     go to agg-ges-mag-450
           else if   w-agg-mag-trt-val    =    "C"
                     go to agg-ges-mag-460
           else if   w-agg-mag-trt-val    =    "X"
                     go to agg-ges-mag-470
           else if   w-agg-mag-trt-val    =    "R"
                     go to agg-ges-mag-480
           else if   w-agg-mag-trt-val    =    "Y"
                     go to agg-ges-mag-490
           else      go to agg-ges-mag-999.
       agg-ges-mag-440.
      *                  *---------------------------------------------*
      *                  * Se trattamento valore : per impostazione di-*
      *                  * retta                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo aggior- *
      *                      * namento                                 *
      *                      *-----------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     go to agg-ges-mag-445.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '+'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione della pre-   *
      *                          * senza o assenza del valore movimento*
      *                          *-------------------------------------*
           if        w-agg-mag-val-mov    =    zero
                     go to agg-ges-mag-442.
      *                          *-------------------------------------*
      *                          * Se valore movimento non a zero      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento quantita' valoriz-*
      *                              * zata nel mese                   *
      *                              *---------------------------------*
           add       w-agg-mag-qta-mov    to   rf-mmv-qtv-mns
                                              (w-agg-mag-mes-ese)     .
      *                              *---------------------------------*
      *                              * Aggiornamento progressivo valore*
      *                              * relativo alla quantita' valoriz-*
      *                              * zata nel mese                   *
      *                              *---------------------------------*
           add       w-agg-mag-val-mov    to   rf-mmv-vlv-mns
                                              (w-agg-mag-mes-ese)     .
      *                              *---------------------------------*
      *                              * Eventuale aggiornamento dati    *
      *                              * mensili relativi al costo ulti- *
      *                              * mo                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Test se da eseguire         *
      *                                  *-----------------------------*
           if        w-agg-mag-cun-mov    =    zero
                     go to agg-ges-mag-441.
           if        w-agg-mag-dat-reg    <    rf-mmv-dcu-mns
                                              (w-agg-mag-mes-ese)
                     go to agg-ges-mag-441.
      *                                  *-----------------------------*
      *                                  * Data costo ultimo del mese  *
      *                                  *-----------------------------*
           move      w-agg-mag-dat-reg    to   rf-mmv-dcu-mns
                                              (w-agg-mag-mes-ese)     .
      *                                  *-----------------------------*
      *                                  * Costo ultimo del mese       *
      *                                  *-----------------------------*
           move      w-agg-mag-cun-mov    to   rf-mmv-ucu-mns
                                              (w-agg-mag-mes-ese)     .
       agg-ges-mag-441.
      *                              *---------------------------------*
      *                              * A continuazione per update      *
      *                              *---------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-442.
      *                          *-------------------------------------*
      *                          * Se valore movimento a zero          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento quantita' in at-  *
      *                              * tesa di valorizzazione nel mese *
      *                              *---------------------------------*
           add       w-agg-mag-qta-mov    to   rf-mmv-qav-mns
                                              (w-agg-mag-mes-ese)     .
      *                              *---------------------------------*
      *                              * A continuazione per update      *
      *                              *---------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-445.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '-'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione della pre-   *
      *                          * senza o assenza del valore movimento*
      *                          *-------------------------------------*
           if        w-agg-mag-val-mov    =    zero
                     go to agg-ges-mag-447.
      *                          *-------------------------------------*
      *                          * Se valore movimento non a zero      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento quantita' valoriz-*
      *                              * zata nel mese                   *
      *                              *---------------------------------*
           subtract  w-agg-mag-qta-mov    from rf-mmv-qtv-mns
                                              (w-agg-mag-mes-ese)     .
      *                              *---------------------------------*
      *                              * Aggiornamento progressivo valore*
      *                              * relativo alla quantita' valoriz-*
      *                              * zata nel mese                   *
      *                              *---------------------------------*
           subtract  w-agg-mag-val-mov    from rf-mmv-vlv-mns
                                              (w-agg-mag-mes-ese)     .
      *                              *---------------------------------*
      *                              * A continuazione per update      *
      *                              *---------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-447.
      *                          *-------------------------------------*
      *                          * Se valore movimento a zero          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento quantita' in at-  *
      *                              * tesa di valorizzazione nel mese *
      *                              *---------------------------------*
           subtract  w-agg-mag-qta-mov    from rf-mmv-qav-mns
                                              (w-agg-mag-mes-ese)     .
      *                              *---------------------------------*
      *                              * A continuazione per update      *
      *                              *---------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-450.
      *                  *---------------------------------------------*
      *                  * Se trattamento valore : al costo standard   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo aggior- *
      *                      * namento                                 *
      *                      *-----------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     go to agg-ges-mag-455.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '+'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento quantita' da valoriz- *
      *                          * zare al costo standard nel mese     *
      *                          *-------------------------------------*
           add       w-agg-mag-qta-mov    to   rf-mmv-qcs-mns
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-455.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '-'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento quantita' da valoriz- *
      *                          * zare al costo standard nel mese     *
      *                          *-------------------------------------*
           subtract  w-agg-mag-qta-mov    from rf-mmv-qcs-mns
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-460.
      *                  *---------------------------------------------*
      *                  * Se trattamento valore : costo aggiuntivo    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo aggior- *
      *                      * namento                                 *
      *                      *-----------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     go to agg-ges-mag-465.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '+'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo costi ag- *
      *                          * giuntivi nel mese                   *
      *                          *-------------------------------------*
           add       w-agg-mag-val-mov    to   rf-mmv-pca-mns
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-465.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '-'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo costi ag- *
      *                          * giuntivi nel mese                   *
      *                          *-------------------------------------*
           subtract  w-agg-mag-val-mov    from rf-mmv-pca-mns
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-470.
      *                  *---------------------------------------------*
      *                  * Se trattamento valore : costo aggiuntivo di *
      *                  * fine esercizio                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo aggior- *
      *                      * namento                                 *
      *                      *-----------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     go to agg-ges-mag-475.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '+'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo costi ag- *
      *                          * giuntivi di chiusura esercizio      *
      *                          *-------------------------------------*
           add       w-agg-mag-val-mov    to   rf-mmv-pca-che         .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-475.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '-'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo costi ag- *
      *                          * giuntivi di chiusura esercizio      *
      *                          *-------------------------------------*
           subtract  w-agg-mag-val-mov    from rf-mmv-pca-che         .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-480.
      *                  *---------------------------------------------*
      *                  * Se trattamento valore : rettifica costo     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo aggior- *
      *                      * namento                                 *
      *                      *-----------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     go to agg-ges-mag-465.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '+'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo rettifi-  *
      *                          * che a costi nel mese                *
      *                          *-------------------------------------*
           add       w-agg-mag-val-mov    to   rf-mmv-prc-mns
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-485.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '-'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo rettifi-  *
      *                          * che a costi nel mese                *
      *                          *-------------------------------------*
           subtract  w-agg-mag-val-mov    from rf-mmv-prc-mns
                                              (w-agg-mag-mes-ese)     .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-490.
      *                  *---------------------------------------------*
      *                  * Se trattamento valore : rettifica costo di  *
      *                  * fine esercizio                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo aggior- *
      *                      * namento                                 *
      *                      *-----------------------------------------*
           if        w-agg-mag-tip-agg    =    "-"
                     go to agg-ges-mag-495.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '+'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo rettifi-  *
      *                          * che a costi di chiusura esercizio   *
      *                          *-------------------------------------*
           add       w-agg-mag-val-mov    to   rf-mmv-prc-che         .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-495.
      *                      *-----------------------------------------*
      *                      * Se aggiornamento in '-'                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo rettifi-  *
      *                          * che a costi di chiusura esercizio   *
      *                          *-------------------------------------*
           subtract  w-agg-mag-val-mov    from rf-mmv-prc-che         .
      *                          *-------------------------------------*
      *                          * A continuazione per update          *
      *                          *-------------------------------------*
           go to     agg-ges-mag-500.
       agg-ges-mag-500.
      *                  *---------------------------------------------*
      *                  * Update record [mmv]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della ope-  *
      *                  * razione di update                           *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to agg-ges-mag-510.
      *                  *---------------------------------------------*
      *                  * Se errori in update                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [mmv]                            *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                      *-----------------------------------------*
      *                      * Ripetizione operazione di aggiornamento *
      *                      * relativa a [mmv]                        *
      *                      *-----------------------------------------*
           go to     agg-ges-mag-410.
       agg-ges-mag-510.
      *                  *---------------------------------------------*
      *                  * Se nessun errore in update                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [mmv]                            *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
       agg-ges-mag-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per operazioni sulle date                     *
      *    *                                                           *
      *    * 'det-dat-nrg-000/999'                                     *
      *    *                                                           *
      *    * Routines per l'aumento di una data in giorni              *
      *    *                                                           *
      *    * Input  : w-det-dat-nrg-dtb = Data iniziale                *
      *    *                                                           *
      *    *          w-det-dat-nrg-ngi = nr. giorni di incremento     *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-det-dat-nrg-dti = Data incrementata            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'det-nrg-dat-000/999'                                     *
      *    *                                                           *
      *    * Routines per la diminuzione di una data in giorni         *
      *    *                                                           *
      *    * Input  : w-det-nrg-dat-dtb = Data iniziale                *
      *    *                                                           *
      *    *          w-det-nrg-dat-ngd = nr. giorni di decremento     *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-det-nrg-dat-dtd = Data decrementata            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cps"                   .

