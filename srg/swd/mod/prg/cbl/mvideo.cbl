       Identification Division.
       Program-Id.                                 mvideo             .
      *================================================================*
      *                                                                *
      *                    Interfaccia gestione video                  *
      *                                                                *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/06/91    *
      *                       Ultima revisione:    NdK del 17/03/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Dichiarazione inizio utilizzo interfaccia video.   *
      *                                                                *
      *             Input  : v-ope = "OP"                              *
      *                                                                *
      *                      v-alf = Sigla del driver video            *
      *                                                                *
      *                      v-edt = pathname del file di appoggio     *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Dichiarazione fine utilizzo interfaccia video.     *
      *                                                                *
      *             Input  : v-ope = "CL"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Azienda     Dichiarazione codice azienda in uso                *
      *                                                                *
      *             Input  : v-ope = "AZ"                              *
      *                                                                *
      *                      v-alf = Codice azienda                    *
      *                                                                *
      *                      v-num = Segnale di richiesta visualizza-  *
      *                              zione codice azienda              *
      *                              - 0 : No                          *
      *                              - 1 : Si                          *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Grafica     Attivazione segnale di modalita' grafica           *
      *                                                                *
      *             Input  : v-ope = "MG"                              *
      *                                                                *
      *                      v-num = Segnale di modalita' grafica      *
      *                              - 0 : Non attiva                  *
      *                              - 1 : Attiva                      *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Azienda     Dichiarazione di richiesta visualizzazione codice  *
      * in alto     azienda in alto a destra in On                     *
      * in On                                                          *
      *             Input  : v-ope = "A+"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Azienda     Dichiarazione di richiesta visualizzazione codice  *
      * in alto     azienda in alto a destra in Off                    *
      * in Off                                                         *
      *             Input  : v-ope = "A-"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Dipendenza  Dichiarazione codice dipendenza in uso             *
      *                                                                *
      *             Input  : v-ope = "DP"                              *
      *                                                                *
      *                      v-car = Numero dipendenze esistenti per   *
      *                              l'azienda                         *
      *                                                                *
      *                      v-num = Codice dipendenza                 *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Set Off     Congelamento visualizzazioni effettive a video.    *
      *                                                                *
      *             Input  : v-ope = "OF"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Set On      Scongelamento visualizzazioni effettive a video.   *
      *                                                                *
      *             Input  : v-ope = "ON"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Save        Salvataggio buffer video su stack su disco.        *
      *                                                                *
      *             Input  : v-ope = "SV"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Restore     Ripristino buffer video da stack su disco e vi-    *
      *             sualizzazione dell'immagine ripristinata .         *
      *                                                                *
      *             Input  : v-ope = "RS"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Fetch Line  Ottenimento immagine linea da buffer video .       *
      *                                                                *
      *             Input  : v-ope = "FL"                              *
      *                                                                *
      *                      v-lin = numero linea interessata          *
      *                                                                *
      *             Output : v-alf = immagine linea interessata        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Accept      Accettazione asincrona di una function key         *
      * Asincrona                                                      *
      *             Input  : v-ope = "AA"                              *
      *                                                                *
      *                      v-lin = Linea per accettazione asincrona, *
      *                              oppure zero per linea standard    *
      *                                                                *
      *                      v-pos = Posizione per accettazione asin-  *
      *                              crona, oppure zero per posizione  *
      *                              standard                          *
      *                                                                *
      *                      v-ufk = Tasti di funzione impostabili da  *
      *                              parte dell'utente (max 10), per   *
      *                              Return usare RTRN                 *
      *                                                                *
      *             Output : v-key = tipo di terminazione accettazione *
      *                              spaces     : accettazione asin-   *
      *                                           crona non eseguita   *
      *                              altrimenti : sigla del tasto di   *
      *                                           funzione usato, con  *
      *                                           Return pari a 'RTRN' *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Accept      Accettazione campo.                                *
      *                                                                *
      *             Input  : v-ope = "AC"                              *
      *                                                                *
      *                      v-tip = tipo campo  : A = alfanumerico    *
      *                                            U = alfa-uppercase  *
      *                                            L = alfa-lowercase  *
      *                                            T = text            *
      *                                            E = espanso         *
      *                                            C = check-list      *
      *                                            N = numerico        *
      *                                            V = valore numerico *
      *                                                con decimali in *
      *                                                forma virtuale  *
      *                                            P = progressivo/anno*
      *                                            D = data            *
      *                                            K = function-key    *
      *                                            W = word-key        *
      *                                                                *
      *                      v-car = se A        : numero di caratteri *
      *                                            max 132             *
      *                              se T        : numero di caratteri *
      *                                            per linea max 132   *
      *                              se E        : numero di caratteri *
      *                                            per elemento max 80 *
      *                              se C        : numero di caratteri *
      *                                            per elemento max 80 *
      *                              se N        : numero di interi    *
      *                                            max 13              *
      *                              se V        : numero di cifre,    *
      *                                            totali, max 13,     *
      *                                            comprensive dei     *
      *                                            decimali virtuali   *
      *                              se P        : numero di interi    *
      *                                            max 10              *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-ldt = se A        : non significativo   *
      *                              se T        : numero linee di te- *
      *                                            sto max 20          *
      *                              se E        : numero elementi     *
      *                              se C        : numero elementi     *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se P        : non significativo   *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-dec = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se E        : non significativo   *
      *                              se C        : non significativo   *
      *                              se N        : numero di decimali  *
      *                                            max 5               *
      *                              se V        : numero di decimali  *
      *                                            virtuali, max 3     *
      *                              se P        : non significativo   *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-sgn = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se C        : non significativo   *
      *                              se E        : non significativo   *
      *                              se N        : segno algebrico     *
      *                                            S =  con   segno    *
      *                                            altrimenti senza    *
      *                              se V        : segno algebrico     *
      *                                            S =  con   segno    *
      *                                            altrimenti senza    *
      *                              se P        : non significativo   *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-edm = se A        : non significativo   *
      *                              se T        : W = Auto Wrap       *
      *                              se E        : M = e' ammesso an-  *
      *                                                che l'elemento  *
      *                                                manuale di in-  *
      *                                                dice zero       *
      *                                            X = forzatura di    *
      *                                                pre-espansione  *
      *                              se C        : X = forzatura di    *
      *                                                pre-espansione  *
      *                                            B = Blank se spaces *
      *                              se N        : attributi editing   *
      *                                            N = nessun editing  *
      *                                            G = raggruppa a tre *
      *                                                a tre i  carat- *
      *                                                teri prima del  *
      *                                                punto decimale  *
      *                                            9 = zeri in testa   *
      *                                            B = blank when zero *
      *                                            D = solo i decimali *
      *                                                significativi   *
      *                                            < = allineamento a  *
      *                                                sinistra        *
      *                                            M = secondo masche- *
      *                                                ra edit v-msk   *
      *                              se V        : attributi editing   *
      *                                            come per numerico   *
      *                              se P        : < = allineamento a  *
      *                                                sinistra        *
      *                                            (in ogni caso sara' *
      *                                            blank when zero)    *
      *                              se D        : > = sono ammesse    *
      *                                            date superiori alla *
      *                                            data di sistema;    *
      *                                            l'editing ha sempre *
      *                                            formato 99/99/99    *
      *                                            S = Anno per esteso *
      *                                                (con secolo):   *
      *                                                solo per la     *
      *                                                vosualizzazione *
      *                                                l'editing e'    *
      *                                                99/99/9999      *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-msk = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se E        : contiene la lista   *
      *                                            dei caratteri ac-   *
      *                                            cettabili e se pre- *
      *                                            sente '#', signifi- *
      *                                            ca che sono accet-  *
      *                                            tabili i relativi   *
      *                                            numeri di posizione *
      *                              se C        : non significativo   *
      *                              se N        : significativo solo  *
      *                                            se v-edm comprende  *
      *                                            il carattere 'M' .  *
      *                                            In questo caso ol-  *
      *                                            tre a 'M' e' ammes- *
      *                                            so solo 'B' .  La   *
      *                                            maschera deve ave-  *
      *                                            re una X per ogni   *
      *                                            intero significati- *
      *                                            va; ogni altro ca-  *
      *                                            rattere viene  in-  *
      *                                            serito brutalmente. *
      *                                            Per esempio con la  *
      *                                            maschera X.X-XXX il *
      *                                            numero 12345 viene  *
      *                                            editato : 1.2-345   *
      *                                            Con la stessa mas-  *
      *                                            chera invece il nu- *
      *                                            mero 345 sarebbe e- *
      *                                            ditato : 0.0-345    *
      *                                            La fine maschera e' *
      *                                            determinata dal pri-*
      *                                            mo carattere spazio.*
      *                                            Per inserire brutal-*
      *                                            mente degli spazi e'*
      *                                            necessario utilizza-*
      *                                            re il carattere 'b' *
      *                              se V        : come per numerico   *
      *                              se P        : non significativo   *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-lin = linea su cui eseguire l'operazio- *
      *                              ne : 01-24                        *
      *                                                                *
      *                      v-pos = posizione iniziale : 01-132       *
      *                                                                *
      *                      v-alf = se A        : valore di default   *
      *                                            del campo alfanume- *
      *                                            rico da accettare   *
      *                              se T        : non significativo   *
      *                              se E        : valore di default   *
      *                                            dell'elemento solo  *
      *                                            se l'indice elemen- *
      *                                            to e' zero ed e'    *
      *                                            ammesso un valore   *
      *                                            manuale             *
      *                              se C        : valore di default   *
      *                                            del campo alfanume- *
      *                                            rico da accettare,  *
      *                                            ridefinito come una *
      *                                            successione di sin- *
      *                                            goli caratteri, per *
      *                                            ognuno dei quali    *
      *                                            vale la seguente    *
      *                                            specifica           *
      *                                              - S : Si          *
      *                                              - N : No          *
      *                                              - ? : No          *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se P        : non significativo   *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-txt = se A        : non significativo   *
      *                              se T        : valore di default   *
      *                                            del campo text da   *
      *                                            accettare           *
      *                              se E        : tabella elementi    *
      *                              se C        : tabella elementi    *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se P        : non significativo   *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-num = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se E        : indice elemento     *
      *                              se C        : non significativo   *
      *                              se N        : valore di default   *
      *                                            del campo numerico  *
      *                                            da accettare        *
      *                              se V        : come per numerico   *
      *                              se P        : valore di default   *
      *                                            del campo da accet- *
      *                                            tare in formato     *
      *                                            s.aa.n---n          *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-dat = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se E        : non significativo   *
      *                              se C        : non significativo   *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se P        : non significativo   *
      *                              se D        : valore di default   *
      *                                            del campo data da   *
      *                                            accettare           *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-ufk = tasti di funzione impostabili da  *
      *                              parte dell'utente. Se questo va-  *
      *                              lore non viene preparato l'uten-  *
      *                              te puo' impostare solo il termi-  *
      *                              natore 'normale' . Al massimo si  *
      *                              possono prevedere 40 terminatori  *
      *                              speciali, ciascuno dei quali vie- *
      *                              ne referenziato come v-pfk(1) ,   *
      *                              v-pfk(2) , v-pfk(3) ecc. ecc.     *
      *                              Vedere anche funzione Add Key .   *
      *                              Se questo valore viene preparato  *
      *                              a spaces in accettazione di una   *
      *                              function-key, qualsiasi carattere *
      *                              fa' terminare l'impostazione e    *
      *                              viene comunque ritornata la key   *
      *                              di uscita Spaces                  *
      *                                                                *
      *             Output : v-alf = se A        : valore del campo    *
      *                                            alfanumerico effet- *
      *                                            tivamente accettato *
      *                              se T        : non significativo   *
      *                              se E        : valore dell'elemen- *
      *                                            to selezionato      *
      *                              se C        : valore del campo    *
      *                                            alfanumerico effet- *
      *                                            tivamente accettato *
      *                                            ridefinito come una *
      *                                            successione di sin- *
      *                                            goli caratteri, per *
      *                                            ognuno dei quali    *
      *                                            vale la seguente    *
      *                                            specifica           *
      *                                              - S : Si          *
      *                                              - N : No          *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se P        : non significativo   *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-txt = se A        : non significativo   *
      *                              se T        : valore del campo    *
      *                                            text effettivamente *
      *                                            accettato           *
      *                              se E        : inalterato          *
      *                              se C        : modificato          *
      *                              se P        : non significativo   *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-num = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se E        : indice dell'elemen- *
      *                                            to selezionato      *
      *                              se C        : non significativo   *
      *                              se N        : valore del campo    *
      *                                            numerico effettiva- *
      *                                            mente accettato     *
      *                              se V        : valore del campo    *
      *                                            numerico effettiva- *
      *                                            mente accettato     *
      *                              se P        : valore del campo ef-*
      *                                            fettivamente accet- *
      *                                            tato nel formato    *
      *                                            s.aa.n---n          *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-dat = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se E        : non significativo   *
      *                              se C        : non significativo   *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se P        : non significativo   *
      *                              se D        : valore del campo    *
      *                                            data effettivamente *
      *                                            accettato           *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-key = tipo di terminazione accettazione *
      *                              spaces     : terminazione normale *
      *                                           Return o Enter       *
      *                              altrimenti : sigla del tasto di   *
      *                                           funzione usato       *
      *                                                                *
      *                      v-mod = segnale se campo modificato in    *
      *                              fase di accettazione              *
      *                              spaces     : no                   *
      *                              altrimenti : si'                  *
      *                              se f-k      : non significativo   *
      *                                                                *
      *                      v-edt = campo editato                     *
      *                              se f-k      : non significativo   *
      *                                                                *
      *                      v-edl = lunghezza del campo editato       *
      *                              se f-k      : non significativo   *
      *                                                                *
      *                      v-sec = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se E        : non significativo   *
      *                              se C        : non significativo   *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se P        : secolo del campo    *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *                      v-ann = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se E        : non significativo   *
      *                              se C        : non significativo   *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se P        : anno del campo      *
      *                              se D        : non significativo   *
      *                              se K        : non significativo   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Add Key     Accoda ulteriore tasto di funzione accettabile .   *
      *                                                                *
      *             Input  : v-ope = "K+"                              *
      *                                                                *
      *                      v-key = sigla tasto funzione da accodare  *
      *                                                                *
      *             Output : v-ufk = arricchito di nuovo tasto         *
      *                                                                *
      *                      v-ope = riportato al valore "AC"          *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Edit        Editing campo alfa o text o numerico o data.       *
      *                                                                *
      *             Input  : v-ope = "ED"                              *
      *                                                                *
      *                      v-tip = come per accept                   *
      *                                                                *
      *                      v-car = come per accept                   *
      *                                                                *
      *                      v-ldt = come per accept                   *
      *                                                                *
      *                      v-dec = come per accept                   *
      *                                                                *
      *                      v-sgn = come per accept                   *
      *                                                                *
      *                      v-edm = come per accept                   *
      *                                                                *
      *                      v-msk = come per accept                   *
      *                                                                *
      *                      v-txt oppure                              *
      *                      v-alf oppure                              *
      *                      v-num oppure                              *
      *                      v-dat = valore del campo da editare       *
      *                                                                *
      *             Output : v-edt = come per accept                   *
      *                                                                *
      *                      v-edl = come per accept                   *
      *                                                                *
      *                      v-sec = come per accept                   *
      *                                                                *
      *                      v-ann = come per accept                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Convert     Conversione campo alfanumerico in numerico         *
      *                                                                *
      *             Input  : v-ope = "CV"                              *
      *                                                                *
      *                      v-car = numero caratteri del campo alfa-  *
      *                              numerico da convertire            *
      *                                                                *
      *                      v-alf = campo alfanumerico da convertire  *
      *                                                                *
      *             Output : v-num = campo numerico corrispondente     *
      *                                                                *
      *                      v-dec = eventuale numero decimali         *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Erase       Cancellazione completa immagine video.             *
      *                                                                *
      *             Input  : v-ope = "ER"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Erase Lines Cancellazione linee video.                         *
      *                                                                *
      *             Input  : v-ope = "EL"                              *
      *                                                                *
      *                      v-lin = linea iniziale inclusa            *
      *                                                                *
      *                      v-lto = linea finale   inclusa            *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Display     Visualizzazione campo                              *
      *                                                                *
      *             Input  : v-ope = "DS"                              *
      *                                                                *
      *                      v-tip = come per accept                   *
      *                                                                *
      *                      v-car = come per accept                   *
      *                                                                *
      *                      v-ldt = come per accept                   *
      *                                                                *
      *                      v-dec = come per accept                   *
      *                                                                *
      *                      v-sgn = come per accept                   *
      *                                                                *
      *                      v-edm = come per accept                   *
      *                                                                *
      *                      v-msk = come per accept                   *
      *                                                                *
      *                      v-lin = come per accept                   *
      *                                                                *
      *                      v-pos = come per accept                   *
      *                                                                *
      *                      v-txt oppure                              *
      *                      v-alf oppure                              *
      *                      v-num oppure                              *
      *                      v-dat = valore del campo da visualizzare  *
      *                                                                *
      *             Output : v-edt = come per accept                   *
      *                                                                *
      *                      v-edl = come per accept                   *
      *                                                                *
      *                      v-sec = come per accept                   *
      *                                                                *
      *                      v-ann = come per accept                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Box         Visualizzazione box internamente vuoto             *
      *                                                                *
      *             Input  : v-ope = "BX"                              *
      *                                                                *
      *                      v-lin = linea   iniziale - superiore      *
      *                                                                *
      *                      v-pos = colonna iniziale - sinistra       *
      *                                                                *
      *                      v-lto = linea   finale   - inferiore      *
      *                                                                *
      *                      v-pto = colonna finale   - destra         *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Linea       Visualizzazione linea continua (madalita' grafica) *
      *                                                                *
      *             Input  : v-ope = "LN"                              *
      *                                                                *
      *                      v-lin = linea   iniziale - superiore      *
      *                                                                *
      *                      v-pos = colonna iniziale - sinistra       *
      *                                                                *
      *                      v-lto = linea   finale   - inferiore      *
      *                                                                *
      *                      v-pto = colonna finale   - destra         *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             ___ DA IMPLEMENTARE ___                            *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Box         Visualizzazione box grafico                        *
      * grafico                                                        *
      *             Input  : v-ope = "BG"                              *
      *                                                                *
      *                      v-lin = linea   iniziale - superiore      *
      *                                                                *
      *                      v-pos = colonna iniziale - sinistra       *
      *                                                                *
      *                      v-lto = linea   finale   - inferiore      *
      *                                                                *
      *                      v-pto = colonna finale   - destra         *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Messaggio   Visualizzazione messaggio di errore .              *
      * Errore con                                                     *
      * Box         Input  : v-ope = "ME"                              *
      *                                                                *
      *                      v-not = messaggio di errore               *
      *                                oppure                          *
      *                              v-nt1 = messaggio per 1. riga     *
      *                              v-nt2 = messaggio per 2. riga     *
      *                                                                *
      *             Output : v-not = riportato a spaces                *
      *                                                                *
      *                      v-nt1 = riportato a spaces                *
      *                                                                *
      *                      v-nt2 = riportato a spaces                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Message     Messaggio alle righe 23 e 24 con accettazione di   *
      * with        una risposta da parte dell'operatore               *
      * reply                                                          *
      *             Input  : v-ope = "MR"                              *
      *                                                                *
      *                      v-not = note da esporre a riga 24 e riga  *
      *                              23 automaticamente a spaces       *
      *                                oppure                          *
      *                              v-nt1 = messaggio per la riga 23  *
      *                              v-nt2 = messaggio per la riga 24  *
      *                                                                *
      *                      v-tip = come per accept                   *
      *                                                                *
      *                      v-car = come per accept                   *
      *                                                                *
      *                      v-dec = come per accept                   *
      *                                                                *
      *                      v-sgn = come per accept                   *
      *                                                                *
      *                      v-edm = come per accept                   *
      *                                                                *
      *                      v-msk = come per accept                   *
      *                                                                *
      *                      v-alf = come per accept                   *
      *                                                                *
      *                      v-num = come per accept                   *
      *                                                                *
      *                      v-dat = come per accept                   *
      *                                                                *
      *                      v-ufk = come per accept                   *
      *                                                                *
      *             Output : v-alf = come per accept                   *
      *                                                                *
      *                      v-num = come per accept                   *
      *                                                                *
      *                      v-dat = come per accept                   *
      *                                                                *
      *                      v-key = come per accept                   *
      *                                                                *
      *                      v-mod = come per accept                   *
      *                                                                *
      *                      v-edt = come per accept                   *
      *                                                                *
      *                      v-edl = come per accept                   *
      *                                                                *
      *                      v-not = riportato a spaces                *
      *                                                                *
      *                      v-nt1 = riportato a spaces                *
      *                                                                *
      *                      v-nt2 = riportato a spaces                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Message     Messaggio alle righe 23 e 24 con accettazione di   *
      * with        una risposta da parte dell'operatore di un carat-  *
      * x-reply     tere alfanumerico che deve essere compreso in una  *
      *             lista dei caratteri di risposta previsti           *
      *                                                                *
      *             Input  : v-ope = "MX"                              *
      *                                                                *
      *                      v-tip = tipo campo alfanumerico           *
      *                              A = alfanumerico normale          *
      *                              U = alfanumerico uppercase        *
      *                              L = alfanumerico lowercase        *
      *                                                                *
      *                      v-not = note da esporre a riga 24 e riga  *
      *                              23 automaticamente a spaces       *
      *                                oppure                          *
      *                              v-nt1 = messaggio per la riga 23  *
      *                              v-nt2 = messaggio per la riga 24  *
      *                                                                *
      *                      v-alf = default della risposta (1 carat.) *
      *                                                                *
      *                      v-msk = lista dei caratteri ammissibili,  *
      *                              se spaces si ammette qualsiasi    *
      *                              carattere ma non un tasto di fun- *
      *                              zione                             *
      *                                                                *
      *                      v-ufk = tasti di funzione impostabili da  *
      *                              parte dell'utente. Se questo va-  *
      *                              lore non viene preparato l'uten-  *
      *                              te puo' impostare solo il termi-  *
      *                              natore 'normale' . Al massimo si  *
      *                              possono prevedere 40 terminatori  *
      *                              speciali, ciascuno dei quali vie- *
      *                              ne referenziato come v-pfk(1) ,   *
      *                              v-pfk(2) , v-pfk(3) ecc. ecc. Se  *
      *                              l'utente preme un tasto funzione  *
      *                              previsto non viene controllato se *
      *                              il carattere impostato appartiene *
      *                              alla lista dei caratteri ammessi  *
      *                                                                *
      *             Output : v-alf = carattere impostato               *
      *                                                                *
      *                      v-key = come per accept                   *
      *                                                                *
      *                      v-mod = come per accept                   *
      *                                                                *
      *                      v-not = riportato a spaces                *
      *                                                                *
      *                      v-nt1 = riportato a spaces                *
      *                                                                *
      *                      v-nt2 = riportato a spaces                *
      *                                                                *
      *                      v-msk = riportato a spaces                *
      *                                                                *
      *                      v-ufk = riportato a spaces                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Tooltip     Visualizzazione note in un box  ___ DA IMPLEM ___  *
      *                                                                *
      *             Input  : v-ope = "TT"                              *
      *                                                                *
      *                      v-txt = note da esporre per un max di 800 *
      *                              caratteri                         *
      *                                                                *
      *                      v-car = lunghezza stringa riga tooltip    *
      *                                                                *
      *                      v-lin = linea   iniziale - superiore      *
      *                                                                *
      *                      v-pos = colonna iniziale - sinistra       *
      *                                                                *
      *                      v-lto = linea   finale   - inferiore      *
      *                                                                *
      *                      v-pto = colonna finale   - destra         *
      *                                                                *
      *             Output : v-txt = riportato a spaces                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Note 1+2    Visualizzazione note alle righe 23 e 24 .          *
      *                                                                *
      *             Input  : v-ope = "NT"                              *
      *                                                                *
      *                      v-not = note da esporre a riga 24 e riga  *
      *                              23 automaticamente a spaces       *
      *                                oppure                          *
      *                              v-nt1 = messaggio per la riga 23  *
      *                              v-nt2 = messaggio per la riga 24  *
      *                                                                *
      *             Output : v-not = riportato a spaces                *
      *                                                                *
      *                      v-nt1 = riportato a spaces                *
      *                                                                *
      *                      v-nt2 = riportato a spaces                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Note 1      Visualizzazione nota alla riga 23 .                *
      *                                                                *
      *             Input  : v-ope = "N1"                              *
      *                                                                *
      *                      v-not = note da esporre a riga 23         *
      *                                                                *
      *             Output : v-not = riportato a spaces                *
      *                                                                *
      *                      v-nt1 = riportato a spaces                *
      *                                                                *
      *                      v-nt2 = riportato a spaces                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Note 2      Visualizzazione nota alla riga 24 .                *
      *                                                                *
      *             Input  : v-ope = "N2"                              *
      *                                                                *
      *                      v-not = note da esporre a riga 24         *
      *                                                                *
      *             Output : v-not = riportato a spaces                *
      *                                                                *
      *                      v-nt1 = riportato a spaces                *
      *                                                                *
      *                      v-nt2 = riportato a spaces                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Cursore     Posizionamento del cursore pre esecuzione stampa   *
      * per                                                            *
      * stampa      Input  : v-ope = "CS"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Messaggio   Messaggio per programma in esecuzione              *
      * programma                                                      *
      * in esecu-   Input  : v-ope = "PE"                              *
      * zione                                                          *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Cancellaz.  Cancellazione messaggio per programma in esecuzio- *
      * messaggio   ne                                                 *
      * programma                                                      *
      * in esecu-   Input  : v-ope = "XE"                              *
      * zione                                                          *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Messaggio   Messaggio per fine esecuzione programma            *
      * fine ese-                                                      *
      * cuzione     Input  : v-ope = "FE"                              *
      * programma                                                      *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Refresh     Visualizzazione contenuto attuale buffer video.    *
      *                                                                *
      *             Input  : v-ope = "RF"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Begin       Dichiarazione inizio ciclo interrogazione .        *
      *                                                                *
      *             Input  : v-ope = "BE"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Begin-Aut   Dichiarazione inizio ciclo interrogazione con ese- *
      *             cuzione 'in automatico'                            *
      *                                                                *
      *             Input  : v-ope = "BA"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Pre M.P.    Abilitazione della funzione pre mark-point         *
      *                                                                *
      *             Input  : v-ope = "PM"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * End         Dichiarazione fine ciclo interrogazione .          *
      *                                                                *
      *             Input  : v-ope = "EN"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Stop        Dichiarazione fine operazioni di interrogazione .  *
      *                                                                *
      *             Input  : v-ope = "ST"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Page        Avanzamento pagina in ciclo di interrogazione .    *
      * Advance                                                        *
      *             Input  : v-ope = "PA"                              *
      *                                                                *
      *             Output : v-key = Esito interazione con operatore   *
      *                              Spaces : Continuare ciclo inter.  *
      *                              "EXIT" : Chiudere   ciclo inter.  *
      *                              "XXXX" : L'operatore ha selezio-  *
      *                                       nato la funzione "XXXX"  *
      *                                       in uno dei mark-point .  *
      *                                                                *
      *                      Se v-key = spaces inoltre :               *
      *                                                                *
      *                              v-lnr = numero linea attuale : 1  *
      *                                                                *
      *                              v-res = numero linee residue : 21 *
      *                                                                *
      *                      Se v-key = "XXXX" inoltre :               *
      *                                                                *
      *                              v-cnt = area associata al punto   *
      *                                      di mark selezionato       *
      *                                                                *
      *                              v-lin = linea del punto di mark   *
      *                                                                *
      *                              v-pos = posiz. del punto di mark  *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Mark        Associazione punto di mark in ciclo interrogazione *
      *                                                                *
      *             Input  : v-ope = "MK"                              *
      *                                                                *
      *                      v-lnr = linea del punto di mark           *
      *                                                                *
      *                      v-pnr = posizione del punto di mark       *
      *                                                                *
      *                      v-cnt = area associata al punto di mark   *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Write       Scrittura linea in ciclo di interrogazione         *
      *                                                                *
      *             Input  : v-ope = "WR"                              *
      *                                                                *
      *                      v-lnr = numero linea da scrivere          *
      *                                                                *
      *                      v-cnt = immagine linea da scrivere        *
      *                                                                *
      *             Output : v-lnr = ( v-lnr + 1 )                     *
      *                                                                *
      *                      v-res = ( 22 - v-lnr )                    *
      *                                                                *
      *                      v-cnt = immagine linea da scrivere ripor- *
      *                              tata a spaces                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Print       Editing singolo campo e suo inserimento a linea e  *
      * Field       posizione in ciclo di interrogazione               *
      *                                                                *
      *             Input  : v-ope = "PF"                              *
      *                                                                *
      *                      v-tip = come per accept                   *
      *                                                                *
      *                      v-car = come per accept                   *
      *                                                                *
      *                      v-ldt = come per accept                   *
      *                                                                *
      *                      v-dec = come per accept                   *
      *                                                                *
      *                      v-sgn = come per accept                   *
      *                                                                *
      *                      v-edm = come per accept, inoltre, se con- *
      *                              tiene il carattere '+' si esegue  *
      *                              un 'Mark' a linea v-lin posizione *
      *                              v-pos con area associata v-cnt    *
      *                                                                *
      *                      v-msk = come per accept                   *
      *                                                                *
      *                      v-lin = come per accept                   *
      *                                                                *
      *                      v-pos = come per accept                   *
      *                                                                *
      *                      v-alf = se A        : valore del campo    *
      *                                            alfanumerico da     *
      *                                            editare e inserire  *
      *                              se T        : non significativo   *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se D        : non significativo   *
      *                                                                *
      *                      v-txt = se A        : non significativo   *
      *                              se T        : valore del campo    *
      *                                            text da editare ed  *
      *                                            inserire            *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se D        : non significativo   *
      *                                                                *
      *                      v-num = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se N        : valore del campo    *
      *                                            numerico da edita-  *
      *                                            re e inserire       *
      *                              se V        : valore del campo    *
      *                                            numerico da edita-  *
      *                                            re e inserire       *
      *                              se D        : non significativo   *
      *                                                                *
      *                      v-dat = se A        : non significativo   *
      *                              se T        : non significativo   *
      *                              se N        : non significativo   *
      *                              se V        : non significativo   *
      *                              se D        : valore del campo    *
      *                                            data da editare e   *
      *                                            inserire            *
      *                                                                *
      *                      v-cnt = area associata al punto di mark   *
      *                              significativa solo se la masche-  *
      *                              ra v-edm contiene un '+'          *
      *                                                                *
      *             Output : v-edt = campo editato                     *
      *                                                                *
      *                      v-edl = lunghezza del campo editato       *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Line Feed   Avanzamento linea in ciclo di interrogazione       *
      *                                                                *
      *             Input  : v-ope = "LF"                              *
      *                                                                *
      *             Output : v-lnr = ( v-lnr + 1 )                     *
      *                                                                *
      *                      v-res = ( 22 - v-lnr )                    *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Vertical    Posizionamento a linea in ciclo di interrogazione  *
      * Positioning                                                    *
      *             Input  : v-ope = "VP"                              *
      *                                                                *
      *                      v-lnr = numero linea a cui posizionarsi   *
      *                                                                *
      *             Output : v-res = ( 22 - v-lnr )                    *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Line-Number Salvataggio numero linea attuale in ciclo di       *
      * Save        interrogazione                                     *
      *                                                                *
      *             Input  : v-ope = "LS"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Line-Number Ripristino numero linea precedentemente salvata    *
      * Restore     in ciclo di interrogazione                         *
      *                                                                *
      *             Input  : v-ope = "LR"                              *
      *                                                                *
      *             Output : v-lnr = ripristinato                      *
      *                                                                *
      *                      v-res = ( 22 - v-lnr )                    *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Get Line    Ottenimento immagine linea in ciclo interrogazione *
      *                                                                *
      *             Input  : v-ope = "GL"                              *
      *                                                                *
      *                      v-lin = numero linea interessato          *
      *                                                                *
      *             Output : v-alf = immagine linea numero v-lin       *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Modo 80     Set del modo di utilizzo a 80 colonne              *
      *                                                                *
      *             Input  : v-ope = "M0"                              *
      *                                                                *
      *             Output : v-ope = Modo di utilizzo precedente       *
      *                               - "M0" : 80 colonne              *
      *                               - "M1" : 132 colonne             *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Modo 132    Set del modo di utilizzo a 132 colonne             *
      *                                                                *
      *             Input  : v-ope = "M1"                              *
      *                                                                *
      *             Output : v-ope = Modo di utilizzo precedente       *
      *                               - "M0" : 80 colonne              *
      *                               - "M1" : 132 colonne             *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Richiesta   Richiesta del modo di utilizzo attuale             *
      * modo di                                                        *
      * utilizzo    Input  : v-ope = "M?"                              *
      * attuale                                                        *
      *             Output : v-ope = Modo di utilizzo attuale          *
      *                               - "M0" : 80 colonne              *
      *                               - "M1" : 132 colonne             *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Disabilita  Inibizione temporanea funzione desk-accessory .    *
      * desk                                                           *
      * accessory   Input  : v-ope = "D-"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Abilita     Riabilitazione funzione desk-accessory .           *
      * desk                                                           *
      * accessory   Input  : v-ope = "D+"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Tipo        Visualizzazione Tipo funzionamento                 *
      * funziona-                                                      *
      * mento       Input  : v-ope = "TF"                              *
      *                                                                *
      *                      v-tfu = Tipo funzionamento                *
      *                      v-tfm = Segnale di modifica avvenuta      *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
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
      *    * Work area per trattamento [xvi] riguardante il cobol      *
      *    *-----------------------------------------------------------*
       01  f-xvi.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-xvi-nam                  pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-xvi-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-xvi-sts                  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Record key per [xvi]                                  *
      *        *-------------------------------------------------------*
           05  f-xvi-key                  pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Immagine del record di [xvi]                          *
      *        *-------------------------------------------------------*
           05  f-xvi-rec.
               10  f-xvi-mod              pic  9(01)                  .
               10  f-xvi-dat.
                   15  f-xvi-lin occurs 24.
                       20  f-xvi-pos occurs 132.
                           25  f-xvi-chr  pic  x(01)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo "mvideo" e driver video  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/z"                                  .

      *    *===========================================================*
      *    * Buffer per l'immagine visibile su video                   *
      *    *-----------------------------------------------------------*
       01  b.
      *        *-------------------------------------------------------*
      *        * Linee                                                 *
      *        *-------------------------------------------------------*
           05  b-lin occurs 24.
      *            *---------------------------------------------------*
      *            * Posizioni                                         *
      *            *---------------------------------------------------*
               10  b-pos occurs 132.
      *                *-----------------------------------------------*
      *                * Caratteri                                     *
      *                *-----------------------------------------------*
                   15  b-chr              pic  x(01)                  .

      *    *===========================================================*
      *    * Nome driver display effettivo                             *
      *    *-----------------------------------------------------------*
       01  d.
      *        *-------------------------------------------------------*
      *        * Prefisso                                              *
      *        *-------------------------------------------------------*
           05  filler                     pic  x(16)
                   value "swd/drv/prg/obj/"                           .
      *        *-------------------------------------------------------*
      *        * Tipo terminale usato                                  *
      *        *-------------------------------------------------------*
           05  d-trm                      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Flag di modulo video aperto/chiuso                    *
      *        * - 0 : chiuso                                          *
      *        * - 1 : aperto                                          *
      *        *-------------------------------------------------------*
           05  w-uno                      pic  9(01) value zero       .
      *        *-------------------------------------------------------*
      *        * Codice azienda in uso                                 *
      *        *-------------------------------------------------------*
           05  w-azi                      pic  x(04) value spaces     .
      *        *-------------------------------------------------------*
      *        * Numero aziende esistenti per l'azienda                *
      *        *-------------------------------------------------------*
           05  w-dpq                      pic  9(02) value zero       .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  w-dpz                      pic  9(04) value zero       .
           05  w-dpz-edt                  pic  9(02) value zero       .
      *        *-------------------------------------------------------*
      *        * Visualizzazione del codice azienda in uso             *
      *        * - 0 : No                                              *
      *        * - 1 : Si                                              *
      *        *-------------------------------------------------------*
           05  w-azv                      pic  9(01) value zero       .
      *        *-------------------------------------------------------*
      *        * Flag di tasto Slct ammissibile                        *
      *        * - Spaces : No                                         *
      *        * - #      : Si                                         *
      *        *-------------------------------------------------------*
           05  w-slc                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di tasto Nxsc ammissibile                        *
      *        * - Spaces : No                                         *
      *        * - #      : Si                                         *
      *        *-------------------------------------------------------*
           05  w-nxt                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di tasto Prsc ammissibile                        *
      *        * - Spaces : No                                         *
      *        * - #      : Si                                         *
      *        *-------------------------------------------------------*
           05  w-prv                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di tasto Back ammissibile                        *
      *        * - Spaces : No                                         *
      *        * - #      : Si                                         *
      *        *-------------------------------------------------------*
           05  w-bck                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di tasto Tab ammissibile                         *
      *        * - Spaces : No                                         *
      *        * - #      : Si                                         *
      *        *-------------------------------------------------------*
           05  w-tab                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di prompt per accettazione funzione visibile     *
      *        * - Spaces : No                                         *
      *        * - #      : Si                                         *
      *        *-------------------------------------------------------*
           05  w-mkf                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento campo di tipo 'V'                 *
      *        *-------------------------------------------------------*
           05  w-tcv                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio numero linea                   *
      *        *-------------------------------------------------------*
           05  w-svl                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio numero posizione               *
      *        *-------------------------------------------------------*
           05  w-svp                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio tipo operazione                *
      *        *-------------------------------------------------------*
           05  w-svo                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Indice di lavoro                                      *
      *        *-------------------------------------------------------*
           05  w-inx                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Puntatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-pnt                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-ctr                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Flag di modalita' grafica attiva                      *
      *        * - 0 : No                                              *
      *        * - 1 : Si                                              *
      *        *-------------------------------------------------------*
           05  w-mdg                      pic  9(01) value zero       .
      *        *-------------------------------------------------------*
      *        * Comodo per editing numero pagina                      *
      *        *-------------------------------------------------------*
           05  w-npg.
               10  w-npg-lit              pic  x(05) value "Pag. "    .
               10  w-npg-num              pic  x(05)                  .
               10  w-npg-r01 redefines
                   w-npg-num.
                   15  w-npg-001          pic  9(01)                  .
                   15  filler             pic  x(04)                  .
               10  w-npg-r02 redefines
                   w-npg-num.
                   15  w-npg-002          pic  9(02)                  .
                   15  filler             pic  x(03)                  .
               10  w-npg-r03 redefines
                   w-npg-num.
                   15  w-npg-003          pic  9(03)                  .
                   15  filler             pic  x(02)                  .
               10  w-npg-r04 redefines
                   w-npg-num.
                   15  w-npg-004          pic  9(04)                  .
                   15  filler             pic  x(01)                  .
               10  w-npg-r05 redefines
                   w-npg-num.
                   15  w-npg-005          pic  9(05)                  .
               10  w-npg-fin              pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per visualizzazione linea se diversa da quella *
      *        * memorizzata                                           *
      *        *-------------------------------------------------------*
           05  w-lse.
               10  w-lse-lin              pic  9(03)                  .
               10  w-lse-imm.
                   15  filler occurs 132  pic  x(01)                  .
               10  w-lse-r10 redefines
                   w-lse-imm.
                   15  w-lse-r1a          pic  x(20)                  .
                   15  w-lse-r1b          pic  x(60)                  .
                   15  w-lse-r1c          pic  x(52)                  .
               10  w-lse-p20              pic  x(20)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      ******************************************************************
       Procedure Division                 using v                     .
      ******************************************************************

      *================================================================*
      *    Main                                                        *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Pre-intervento per visualizzazione di :         *
      *              *                                                 *
      *              *  - Livello di esecuzione desk accessory a       *
      *              *    linea 01 posizione 35                        *
      *              *  - Sigla azienda e/o dipendenza a linea 02      *
      *              *    posizione 01                                 *
      *              *-------------------------------------------------*
       main-025.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione diverso da :             *
      *                  * - 'DS' : Display                            *
      *                  * - 'PF' : Print Field                        *
      *                  * no intervento                               *
      *                  *---------------------------------------------*
           if        v-ope                not  = "DS" and
                     v-ope                not  = "PF"
                     go to main-200.
      *                  *---------------------------------------------*
      *                  * Se linea 01 : a trattamento linea 01        *
      *                  * Se linea 02 : a trattamento linea 02        *
      *                  * Altrimenti  : nessun trattamento            *
      *                  *---------------------------------------------*
           if        v-lin                =    01
                     go to main-050
           else if   v-lin                =    02
                     go to main-075
           else      go to main-200.
       main-050.
      *                  *---------------------------------------------*
      *                  * Se linea 01                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se posizione interessata diversa da 01  *
      *                      * non si esegue alcun intervento          *
      *                      *-----------------------------------------*
           if        v-pos                not  = 01
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Se il tipo campo non e' alfabetico non  *
      *                      * si esegue alcun intervento              *
      *                      *-----------------------------------------*
           if        v-tip                not  = "A"
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Se la lunghezza del campo non e' pari   *
      *                      * ad una linea intera non si esegue alcun *
      *                      * intervento                              *
      *                      *-----------------------------------------*
           if        v-car                not  = z-mmx
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Se il segnale per la manipolazione del  *
      *                      * codice azienda in alto a sinistra e' in *
      *                      * Off : non si esegue alcun intervento    *
      *                      *-----------------------------------------*
           if        z-mca                =    zero
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Se non si e' in esecuzione di un desk-  *
      *                      * accessory non si esegue alcun interven- *
      *                      * to                                      *
      *                      *-----------------------------------------*
           if        z-lid                <    "01" or
                     z-lid                >    "99"
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Se il desk-accessory e' di tipo SHCP,   *
      *                      * ovvero Screen-Hard-Copy, non si esegue  *
      *                      * alcun intervento                        *
      *                      *-----------------------------------------*
           if        z-tda                =    "SHCP"
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Intervento su linea 01                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si pone al centro della linea il    *
      *                          * literal {Livello nn}                *
      *                          *-------------------------------------*
           move      z-mmx                to   w-pnt                  .
           subtract  4                    from w-pnt                  .
           divide    2                    into w-pnt                  .
           add       1                    to   w-pnt                  .
           move      "{"                  to   v-alf (w-pnt : 1)      .
           add       1                    to   w-pnt                  .
           move      z-lid                to   v-alf (w-pnt : 2)      .
           add       2                    to   w-pnt                  .
           move      "}"                  to   v-alf (w-pnt : 1)      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     main-200.
       main-075.
      *                  *---------------------------------------------*
      *                  * Se linea 02                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se posizione interessata diversa da 01  *
      *                      * non si esegue alcun intervento          *
      *                      *-----------------------------------------*
           if        v-pos                not  = 01
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Se il tipo campo non e' alfabetico non  *
      *                      * si esegue alcun intervento              *
      *                      *-----------------------------------------*
           if        v-tip                not  = "A"
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Se il campo alfabetico vale Spaces non  *
      *                      * si esegue alcun intervento              *
      *                      *-----------------------------------------*
           if        v-alf                =    spaces
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Se il segnale per la manipolazione del  *
      *                      * codice azienda in alto a sinistra e' in *
      *                      * Off : non si esegue alcun intervento    *
      *                      *-----------------------------------------*
           if        z-mca                =    zero
                     go to main-200.
      *                      *-----------------------------------------*
      *                      * Intervento su linea 02                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio valore attuale di v-alf *
      *                          *-------------------------------------*
           move      v-alf                to   w-lse-imm              .
      *                          *-------------------------------------*
      *                          * Manipolazione immagine relativa al- *
      *                          * la linea 02 per inserimento codice  *
      *                          * azienda e codice dipendenza         *
      *                          *-------------------------------------*
           perform   l02-000              thru l02-999                .
      *                          *-------------------------------------*
      *                          * Ripristino valore di v-alf          *
      *                          *-------------------------------------*
           move      w-lse-imm            to   v-alf                  .
      *                          *-------------------------------------*
      *                          * Eventuale modifica della lunghezza  *
      *                          * del campo da visualizzare           *
      *                          *-------------------------------------*
           if        v-car                <    20
                     move  20             to   v-car                  .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     main-200.
       main-200.
      *              *-------------------------------------------------*
      *              * Se tipo operazione = - Display                  *
      *              *                      - Note A Righe 23 e 24     *
      *              *                      - Note A Riga  23          *
      *              *                      - Note A Riga  24          *
      *              *                      - Edit                     *
      *              *                      - Accept                   *
      *              *                      - Box                      *
      *              *                      - Messaggio Errore         *
      *              *                      - Messaggio Con Risposta   *
      *              *                      - Erase                    *
      *              *                      - Set On                   *
      *              *                      - Refresh                  *
      *              *                      - Convert                  *
      *              *                      - Tipo funzionamento       *
      *              *-------------------------------------------------*
           if        v-ope                =    "AA" or
                     v-ope                =    "DS" or
                     v-ope                =    "NT" or
                     v-ope                =    "N1" or
                     v-ope                =    "N2" or
                     v-ope                =    "ED" or
                     v-ope                =    "AC" or
                     v-ope                =    "BG" or
                     v-ope                =    "BX" or
                     v-ope                =    "ME" or
                     v-ope                =    "MR" or
                     v-ope                =    "MX" or
                     v-ope                =    "ER" or
                     v-ope                =    "EL" or
                     v-ope                =    "ON" or
                     v-ope                =    "RF" or
                     v-ope                =    "CV" or
                     v-ope                =    "SV" or
                     v-ope                =    "RS" or
                     v-ope                =    "M0" or
                     v-ope                =    "M1" or
                     v-ope                =    "M?" or
                     v-ope                =    "D-" or
                     v-ope                =    "D+" or
                     v-ope                =    "TF"
                     call    d           using z
                                               b
                                               v
                                               f-xvi
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Add Key                    *
      *              *-------------------------------------------------*
           else if   v-ope                =    "K+"
                     perform adk-000      thru adk-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Fetch Line                 *
      *              *-------------------------------------------------*
           else if   v-ope                =    "FL"
                     perform ftl-000      thru ftl-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Set Off                    *
      *              *-------------------------------------------------*
           else if   v-ope                =    "OF"
                     perform sof-000      thru sof-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Open                       *
      *              *-------------------------------------------------*
           else if   v-ope                =    "OP"
                     perform opn-000      thru opn-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Close                      *
      *              *-------------------------------------------------*
           else if   v-ope                =    "CL"
                     perform cls-000      thru cls-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Cursore per stampa         *
      *              *-------------------------------------------------*
           else if   v-ope                =    "CS"
                     perform cps-000      thru cps-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Messaggio di programma in  *
      *              *                      esecuzione                 *
      *              *-------------------------------------------------*
           else if   v-ope                =    "PE"
                     perform pie-000      thru pie-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Cancellazione messaggio di *
      *              *                      programma in esecuzione    *
      *              *-------------------------------------------------*
           else if   v-ope                =    "XE"
                     perform pix-000      thru pix-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Fine esecuzione programma  *
      *              *-------------------------------------------------*
           else if   v-ope                =    "FE"
                     perform fep-000      thru fep-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Dichiarazione del codice   *
      *              *                      azienda in uso             *
      *              *-------------------------------------------------*
           else if   v-ope                =    "AZ"
                     perform azi-000      thru azi-999
      *              *-------------------------------------------------*
      *              * Attivazione segnale di modalita' grafica        *
      *              *-------------------------------------------------*
           else if   v-ope                =    "MG"
                     perform mdg-000      thru mdg-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Dichiarazione del codice   *
      *              *                      dipendenza in uso          *
      *              *-------------------------------------------------*
           else if   v-ope                =    "DP"
                     perform dpz-000      thru dpz-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Manipolazione codice a-    *
      *              *                      zienda in On               *
      *              *-------------------------------------------------*
           else if   v-ope                =    "A+"
                     perform aon-000      thru aon-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Manipolazione codice a-    *
      *              *                      zienda in Off              *
      *              *-------------------------------------------------*
           else if   v-ope                =    "A-"
                     perform aof-000      thru aof-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Di Ciclo Interrogazione    *
      *              *-------------------------------------------------*
           else      perform qry-000      thru qry-999                .
       main-999.
           exit      program                                          .

      *================================================================*
      *    Open                                                        *
      *----------------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Composizione name per file [xvi]                *
      *              *-------------------------------------------------*
           move      "xvi "               to   f-xvi-nam              .
      *              *-------------------------------------------------*
      *              * Composizione pathname per file [xvi]            *
      *              *-------------------------------------------------*
           move      v-edt                to   f-xvi-pat              .
      *              *-------------------------------------------------*
      *              * Test se modulo gia' aperto                      *
      *              *-------------------------------------------------*
           if        w-uno                not  = zero
                     go to opn-999.
      *              *-------------------------------------------------*
      *              * Flag di modulo gia' aperto                      *
      *              *-------------------------------------------------*
           move      1                    to   w-uno                  .
      *              *-------------------------------------------------*
      *              * Modo di utilizzo : 80 colonne                   *
      *              *-------------------------------------------------*
           move      0                    to   z-mod                  .
      *              *-------------------------------------------------*
      *              * Max numero di colonne : 80                      *
      *              *-------------------------------------------------*
           move      080                  to   z-mmx                  .
      *              *-------------------------------------------------*
      *              * Ultimo modo di utilizzo effettivamente attivato *
      *              * : 80 colonne                                    *
      *              *-------------------------------------------------*
           move      0                    to   z-umu                  .
      *              *-------------------------------------------------*
      *              * Manipolazione per codice azienda in alto a de-  *
      *              * stra : in On                                    *
      *              *-------------------------------------------------*
           move      1                    to   z-mca                  .
      *              *-------------------------------------------------*
      *              * Max record number file appoggio = zero          *
      *              *-------------------------------------------------*
           move      zero                 to   z-rcn                  .
      *              *-------------------------------------------------*
      *              * Livello di on/off a zero                        *
      *              *-------------------------------------------------*
           move      zero                 to   z-ooo                  .
      *              *-------------------------------------------------*
      *              * Linee in sospeso trattate in Off = nessuna      *
      *              *-------------------------------------------------*
           move      low-values           to   z-oob                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione clipboard                      *
      *              *  - Alfanumerico                                 *
      *              *  - Text                                         *
      *              *  - Numerico                                     *
      *              *  - Data                                         *
      *              *-------------------------------------------------*
           move      spaces               to   z-cla
                                               z-clt                  .
           move      zero                 to   z-cln
                                               z-cld                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione function keys previste         *
      *              *-------------------------------------------------*
           move      spaces               to   v-ufk                  .
      *              *-------------------------------------------------*
      *              * Buffer immagine visibile = spaces               *
      *              *-------------------------------------------------*
           move      spaces               to   b                      .
      *              *-------------------------------------------------*
      *              * Composizione nome driver display effettivo      *
      *              *-------------------------------------------------*
           move      v-alf                to   d-trm                  .
      *              *-------------------------------------------------*
      *              * Richiamo driver per funzione Open               *
      *              *-------------------------------------------------*
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       opn-999.
           exit.

      *================================================================*
      *    Close                                                       *
      *----------------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Test se modulo gia' chiuso                      *
      *              *-------------------------------------------------*
           if        w-uno                =    zero
                     go to cls-999.
      *              *-------------------------------------------------*
      *              * Flag di modulo gia' chiuso                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-uno                  .
      *              *-------------------------------------------------*
      *              * Richiamo driver per funzione Close              *
      *              *-------------------------------------------------*
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       cls-999.
           exit.

      *================================================================*
      *    Scrittura record f-xvi-rec in file appoggio, con completa-  *
      *    mento del record con il modo di utilizzo attuale, 80 o 132  *
      *----------------------------------------------------------------*
       imw-000.
      *              *-------------------------------------------------*
      *              * Salvataggio tipo operazione                     *
      *              *-------------------------------------------------*
           move      v-ope                to   w-svo                  .
      *              *-------------------------------------------------*
      *              * Completamento record con preparazione del modo  *
      *              * di utilizzo attuale, 80 o 132 colonne           *
      *              *-------------------------------------------------*
           move      z-mod                to   f-xvi-mod              .
      *              *-------------------------------------------------*
      *              * Richiamo driver per esecuzione vera e propria   *
      *              *-------------------------------------------------*
           move      "IW"                 to   v-ope                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
      *              *-------------------------------------------------*
      *              * Ripristino tipo operazione                      *
      *              *-------------------------------------------------*
           move      w-svo                to   v-ope                  .
       imw-999.
           exit.

      *================================================================*
      *    Lettura record f-xvi-rec da file appoggio, con eventuale    *
      *    ripristino del modo di utilizzo salvato al momento della    *
      *    scrittura                                                   *
      *----------------------------------------------------------------*
       imr-000.
      *              *-------------------------------------------------*
      *              * Salvataggio tipo operazione                     *
      *              *-------------------------------------------------*
           move      v-ope                to   w-svo                  .
      *              *-------------------------------------------------*
      *              * Richiamo driver per esecuzione vera e propria   *
      *              *-------------------------------------------------*
           move      "IR"                 to   v-ope                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
      *              *-------------------------------------------------*
      *              * Ripristino, se necessario, del modo di utilizzo *
      *              *-------------------------------------------------*
           if        f-xvi-mod            =    1
                     move  "M1"           to   v-ope
           else      move  "M0"           to   v-ope                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
      *              *-------------------------------------------------*
      *              * Ripristino tipo operazione                      *
      *              *-------------------------------------------------*
           move      w-svo                to   v-ope                  .
       imr-999.
           exit.

      *================================================================*
      *    Pura lettura record f-xvi-rec da file appoggio, senza al-   *
      *    cuna ulteriore azione                                       *
      *----------------------------------------------------------------*
       iml-000.
      *              *-------------------------------------------------*
      *              * Salvataggio tipo operazione                     *
      *              *-------------------------------------------------*
           move      v-ope                to   w-svo                  .
      *              *-------------------------------------------------*
      *              * Richiamo driver per esecuzione vera e propria   *
      *              *-------------------------------------------------*
           move      "IR"                 to   v-ope                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
      *              *-------------------------------------------------*
      *              * Ripristino tipo operazione                      *
      *              *-------------------------------------------------*
           move      w-svo                to   v-ope                  .
       iml-999.
           exit.

      *================================================================*
      *    Rivisualizzazione selettiva con memorizzazione              *
      *----------------------------------------------------------------*
       rsm-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri display tranne v-lin     *
      *              *                                     e v-alf     *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      z-mmx                to   v-car                  .
           move      01                   to   v-pos                  .
      *              *-------------------------------------------------*
      *              * Ripristino e visualizzazione delle linee che    *
      *              * differiscono da quelle attuali                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-inx                  .
       rsm-500.
           add       1                    to   w-inx                  .
           if        w-inx                >    24
                     go to rsm-999.
           if        f-xvi-lin(w-inx)     =    b-lin(w-inx)
                     go to rsm-500.
           move      w-inx                to   v-lin                  .
           move      f-xvi-lin(w-inx)     to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
           go to     rsm-500.
       rsm-999.
           exit.
           
      *================================================================*
      *    Fetch Line                                                  *
      *----------------------------------------------------------------*
       ftl-000.
      *              *-------------------------------------------------*
      *              * Estrazione linea da buffer del video            *
      *              *-------------------------------------------------*
           move      b-lin(v-lin)         to   v-alf                  .
       ftl-999.
           exit.

      *================================================================*
      *    Set Off                                                     *
      *----------------------------------------------------------------*
       sof-000.
      *              *-------------------------------------------------*
      *              * Decremento del livello di 'on'                  *
      *              *-------------------------------------------------*
           subtract  1                    from z-ooo                  .
       sof-999.
           exit.

      *================================================================*
      *    Add Key                                                     *
      *----------------------------------------------------------------*
       adk-000.
      *              *-------------------------------------------------*
      *              * Ricerca di una occorrenza a spaces e, se trova- *
      *              * ta, suo rimpiazzamento con la sigla della  fun- *
      *              * ction-key                                       *
      *              *-------------------------------------------------*
           set       v-pfx                to   1                      .
           search    v-pfk
                     when   v-pfk
                           (v-pfx)        =    spaces
                            move  v-key   to   v-pfk
                                              (v-pfx)                 .
           move      "AC"                 to   v-ope                  .
       adk-999.
           exit.

      *================================================================*
      *    Posizionamento del cursore pre-esecuzione stampa            *
      *----------------------------------------------------------------*
       cps-000.
      *              *-------------------------------------------------*
      *              * Posizionamento del cursore tramite display      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "="                  to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       cps-999.
           exit.

      *================================================================*
      *    Messaggio di programma in esecuzione                        *
      *----------------------------------------------------------------*
       pie-000.
      *              *-------------------------------------------------*
      *              * Cancellazione linea 23                          *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      z-mmx                to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       pie-100.
      *              *-------------------------------------------------*
      *              * Messaggio a linea 24                            *
      *              *                                                 *
      *              * Differenziato a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
       pie-110.
           if        z-mod                =    1
                     go to pie-130.
       pie-120.
      *                  *---------------------------------------------*
      *                  * 80 colonne                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      z-mmx                to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                       *** [ ELABORAZIONE IN CORSO
      -              " ] ***                        "
                                          to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
           go to     pie-200.
       pie-130.
      *                  *---------------------------------------------*
      *                  * 132 colonne                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      z-mmx                to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                                **
      -              "* [ ELABORAZIONE IN CORSO ] ***                   
      -              "                                "
                                          to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
           go to     pie-200.
       pie-200.
      *              *-------------------------------------------------*
      *              * Posizionamento cursore pre-esecuzione stampa    *
      *              *-------------------------------------------------*
           perform   cps-000              thru cps-999                .
       pie-999.
           exit.

      *================================================================*
      *    Cancellazione messaggio di programma in esecuzione          *
      *----------------------------------------------------------------*
       pix-000.
      *              *-------------------------------------------------*
      *              * Cancellazione linea 24                          *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      z-mmx                to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       pix-999.
           exit.

      *================================================================*
      *    Messaggio di fine esecuzione programma                      *
      *----------------------------------------------------------------*
       fep-000.
      *              *-------------------------------------------------*
      *              * Cancellazione linea 23                          *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      z-mmx                to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       fep-100.
      *              *-------------------------------------------------*
      *              * Messaggio a linea 24                            *
      *              *                                                 *
      *              * Differenziato a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
       fep-110.
           if        z-mod                =    1
                     go to fep-130.
       fep-120.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      z-mmx                to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                      ---[ Fine esecuzione program
      -              "ma ]---                   [ ] "
                                          to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
           go to     fep-200.
       fep-130.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      z-mmx                to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                                --
      -              "-[ Fine esecuzione programma ]---                 
      -              "                            [ ] "
                                          to   v-txt                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
           go to     fep-200.
       fep-200.
      *              *-------------------------------------------------*
      *              * Accettazione di un qualsiasi carattere a linea  *
      *              * 24, tutto a destra                              *
      *              *                                                 *
      *              * Differenziato a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      24                   to   v-lin                  .
           move      z-mmx                to   v-pos                  .
           subtract  2                    from v-pos                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       fep-999.
           exit.

      *================================================================*
      * Dichiarazione del codice azienda in uso                        *
      *                                                                *
      * Nota : questa funzione viene solamente richiamata dal modulo   *
      *        di segreteria alla funzione "AZ"                        *
      *----------------------------------------------------------------*
       azi-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione del codice azienda               *
      *              *-------------------------------------------------*
           move      v-alf                to   w-azi                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione della richiesta di visualizza-   *
      *              * zione del codice azienda                        *
      *              *-------------------------------------------------*
           move      v-num                to   w-azv                  .
       azi-999.
           exit.

      *================================================================*
      * Attivazione eventuale del segnale di modalita' grafica         *
      *                                                                *
      * Nota : questa funzione viene solamente richiamata dal modulo   *
      *        di segreteria alla funzione "MG"                        *
      *----------------------------------------------------------------*
       mdg-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione del segnale di modalita' grafica *
      *              *-------------------------------------------------*
           move      v-num                to   w-mdg                  .
           move      w-mdg                to   z-mdg                  .
      *              *-------------------------------------------------*
      *              * Normalizzazione switch di Box in sospeso        *
      *              *-------------------------------------------------*
           move      0                    to   z-bxs                  .
       mdg-999.
           exit.

      *================================================================*
      * Manipolazione del codice azienda in On                         *
      *----------------------------------------------------------------*
       aon-000.
      *              *-------------------------------------------------*
      *              * Set del segnale in On                           *
      *              *-------------------------------------------------*
           move      1                    to   z-mca                  .
       aon-999.
           exit.

      *================================================================*
      * Manipolazione del codice azienda in Off                        *
      *----------------------------------------------------------------*
       aof-000.
      *              *-------------------------------------------------*
      *              * Set del segnale in Off                          *
      *              *-------------------------------------------------*
           move      zero                 to   z-mca                  .
       aof-999.
           exit.

      *================================================================*
      * Dichiarazione del codice dipendenza in uso                     *
      *                                                                *
      * Nota : questa funzione viene solamente richiamata dal modulo   *
      *        di segreteria alla funzione "DP"                        *
      *----------------------------------------------------------------*
       dpz-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione numero dipendenze esistenti per  *
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           move      v-car                to   w-dpq                  .
      *              *-------------------------------------------------*
      *              * Se codice dipendenza pari a quello precedente : *
      *              * nessuna azione                                  *
      *              *-------------------------------------------------*
           if        v-num                =    w-dpz
                     go to dpz-999.
       dpz-200.
      *              *-------------------------------------------------*
      *              * Memorizzazione del codice dipendenza            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In comodi di trattamento                    *
      *                  *---------------------------------------------*
           move      v-num                to   w-dpz                  .
           move      w-dpz                to   w-dpz-edt              .
       dpz-300.
      *              *-------------------------------------------------*
      *              * Ricomposizione di linea 02 positions 01..20     *
      *              * per rivisualizzare il codice dipendenza         *
      *              *-------------------------------------------------*
           move      b-lin(02)            to   w-lse-imm              .
           move      w-lse-r1a            to   w-lse-p20              .
       dpz-310.
           move      zero                 to   w-pnt                  .
           inspect   w-lse-p20        tallying w-pnt
                                          for  all "]"                .
           if        w-pnt                =    zero
                     go to dpz-320.
           inspect   w-lse-p20       replacing
                                    characters by spaces
                                        before initial "]"            .
           inspect   w-lse-p20       replacing
                                          all  "]" by spaces          .
       dpz-320.
           move      zero                 to   w-pnt                  .
           inspect   w-lse-p20        tallying w-pnt
                                          for  all ")"                .
           if        w-pnt                =    zero
                     go to dpz-330.
           inspect   w-lse-p20       replacing
                                    characters by spaces
                                        before initial ")"            .
           inspect   w-lse-p20       replacing
                                          all  ")" by spaces          .
       dpz-330.
           move      zero                 to   w-pnt                  .
           inspect   w-lse-p20        tallying w-pnt
                                          for  leading spaces         .
           add       1                    to   w-pnt                  .
           move      spaces               to   w-lse-r1a              .
           if        w-pnt                >    20
                     go to dpz-340.
           move      w-lse-p20 (w-pnt : ) to   w-lse-r1a              .
       dpz-340.
           perform   l02-000              thru l02-999                .
       dpz-400.
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione linea 02, primi venti     *
      *                  * caratteri                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-lse-r1a            to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       dpz-999.
           exit.

      *    *===========================================================*
      *    * Manipolazione per immagine relativa alla linea 02 per     *
      *    * inserimento codice azienda e codice dipendenza            *
      *    *-----------------------------------------------------------*
       l02-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda delle personalizzazioni    *
      *              * che indicano se la visualizzazione del codice   *
      *              * azienda e' richiesta oppure no                  *
      *              *-------------------------------------------------*
           if        w-azv                not  = zero
                     go to l02-600.
       l02-200.
      *              *-------------------------------------------------*
      *              * Se la visualizzazione del codice azienda non    *
      *              * e' richiesta                                    *
      *              *-------------------------------------------------*
       l02-225.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore del codice  *
      *                  * dipendenza in uso, e del numero di dipen-   *
      *                  * denze esistenti per l'azienda               *
      *                  *---------------------------------------------*
           if        w-dpq                <    2 or
                     w-dpz                =    zero
                     go to l02-275.
       l02-250.
      *                  *---------------------------------------------*
      *                  * Se numero dipendenze esistenti per l'azien- *
      *                  * da non inferiore a due ed inoltre codice    *
      *                  * dipendenza in uso diverso da zero           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Manipolazione stringa                   *
      *                      *-----------------------------------------*
           move      spaces               to   w-lse-p20              .
           string    "("
                                delimited by   size
                     w-dpz-edt
                                delimited by   size
                     ")  "
                                delimited by   size
                     w-lse-r1a
                                delimited by   spaces
                                          into w-lse-p20              .
           move      w-lse-p20            to   w-lse-r1a              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     l02-999.
       l02-275.
      *                  *---------------------------------------------*
      *                  * Se numero dipendenze esistenti per l'azien- *
      *                  * da inferiore a due oppure se codice dipen-  *
      *                  * denza in uso pari a zero                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita senza alcuna azione              *
      *                      *-----------------------------------------*
           go to     l02-999.
       l02-600.
      *              *-------------------------------------------------*
      *              * Se la visualizzazione del codice azienda e'     *
      *              * richiesta                                       *
      *              *-------------------------------------------------*
       l02-625.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore del codice  *
      *                  * dipendenza in uso                           *
      *                  *---------------------------------------------*
           if        w-dpq                <    2 or
                     w-dpz                =    zero
                     go to l02-675.
       l02-650.
      *                  *---------------------------------------------*
      *                  * Se numero dipendenze esistenti per l'azien- *
      *                  * da non inferiore a due ed inoltre codice    *
      *                  * dipendenza in uso diverso da zero           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Manipolazione stringa                   *
      *                      *-----------------------------------------*
           move      spaces               to   w-lse-p20              .
           string    "["
                                delimited by   size
                     w-azi
                                delimited by   spaces
                     "].("
                                delimited by   size
                     w-dpz-edt
                                delimited by   size
                     ") "
                                delimited by   size
                     w-lse-r1a
                                delimited by   spaces
                                          into w-lse-p20              .
           move      w-lse-p20            to   w-lse-r1a              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     l02-999.
       l02-675.
      *                  *---------------------------------------------*
      *                  * Se numero dipendenze esistenti per l'azien- *
      *                  * da inferiore a due oppure se codice dipen-  *
      *                  * denza in uso pari a zero                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Manipolazione stringa                   *
      *                      *-----------------------------------------*
           move      spaces               to   w-lse-p20              .
           string    "["
                                delimited by   size
                     w-azi
                                delimited by   spaces
                     "]  "
                                delimited by   size
                     w-lse-r1a
                                delimited by   spaces
                                          into w-lse-p20              .
           move      w-lse-p20            to   w-lse-r1a              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     l02-999.
       l02-999.
           exit.

      *    *===========================================================*
      *    * Query routines                                            *
      *    *-----------------------------------------------------------*
       qry-000.
      *              *-------------------------------------------------*
      *              * Selezione tipo funzione di interrogazione       *
      *              *-------------------------------------------------*
           if        v-ope                =    "WR"
                     go to qry-050
           else if   v-ope                =    "PF"
                     go to qry-150
           else if   v-ope                =    "MK"
                     go to qry-040
           else if   v-ope                =    "LF"
                     go to qry-060
           else if   v-ope                =    "PA"
                     go to qry-200
           else if   v-ope                =    "VP"
                     go to qry-070
           else if   v-ope                =    "LS"
                     go to qry-090
           else if   v-ope                =    "LR"
                     go to qry-100
           else if   v-ope                =    "GL"
                     go to qry-080
           else if   v-ope                =    "BE" or
                     v-ope                =    "BA"
                     go to qry-010
           else if   v-ope                =    "PM"
                     go to qry-015
           else if   v-ope                =    "EN"
                     go to qry-020
           else if   v-ope                =    "ST"
                     go to qry-030
           else      go to qry-999.
       qry-010.
      *              *=================================================*
      *              * Funzione "BE" : Begin                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio max rec number file appoggio    *
      *                  *---------------------------------------------*
           move      z-rcn                to   v-qsr                  .
      *                     *------------------------------------------*
      *                     * Bufferizzazione max 10 f-keys ammissibi- *
      *                     * li passate dal programma chiamante       *
      *                     *------------------------------------------*
           move      v-ufk                to   v-qsu                  .
      *                     *------------------------------------------*
      *                     * Conteggio numero f-keys ammissibili      *
      *                     *------------------------------------------*
           move      10                   to   v-qsn                  .
       qry-012.
           if        v-qsn                >    zero
                     if  v-pfk (v-qsn)    =    spaces
                         subtract    1    from v-qsn
                         go to qry-012.
      *                     *------------------------------------------*
      *                     * Inizializzazione numero pagina           *
      *                     *------------------------------------------*
           move      zero                 to   v-pag                  .
      *                     *------------------------------------------*
      *                     * Set 'stop' non ancora eseguito           *
      *                     *------------------------------------------*
           move      low-value            to   v-q99                  .
      *                     *------------------------------------------*
      *                     * Set off/on funzionamento automatico      *
      *                     *------------------------------------------*
           if        v-ope                =    "BA"
                     move   "9"           to   v-qau
           else      move   "0"           to   v-qau                  .
      *                     *------------------------------------------*
      *                     * Marker interrogazione a spaces           *
      *                     *------------------------------------------*
           move      spaces               to   v-mrk                  .
      *                     *------------------------------------------*
      *                     * Function keys a spaces                   *
      *                     *------------------------------------------*
           move      spaces               to   v-ufk                  .
      *                     *------------------------------------------*
      *                     * Segnale di abilitazione pre mark-point   *
      *                     *------------------------------------------*
           move      spaces               to   v-qpm                  .
      *                     *------------------------------------------*
      *                     * Segnale di rientro da pre mark-point     *
      *                     *------------------------------------------*
           move      spaces               to   v-qrm                  .
      *                     *------------------------------------------*
      *                     * Segnali liberi a No                      *
      *                     *------------------------------------------*
           move      spaces               to   v-ql1                  .
           move      spaces               to   v-ql2                  .
      *                     *------------------------------------------*
      *                     * Uscita                                   *
      *                     *------------------------------------------*
           go to     qry-999.
       qry-015.
      *              *=================================================*
      *              * Funzione "PM" : Abilitazione pre mark-point     *
      *              *-------------------------------------------------*
      *                     *------------------------------------------*
      *                     * Segnale di abilitazione pre mark-point a *
      *                     * Si                                       *
      *                     *------------------------------------------*
           move      "S"                  to   v-qpm                  .
      *                     *------------------------------------------*
      *                     * Uscita                                   *
      *                     *------------------------------------------*
           go to     qry-999.
       qry-020.
      *              *=================================================*
      *              * Funzione "EN" : End                             *
      *              *-------------------------------------------------*
      *                     *------------------------------------------*
      *                     * Ripristino max rec number file appoggio  *
      *                     *------------------------------------------*
           move      v-qsr                to   z-rcn                  .
           go to     qry-999.
       qry-030.
      *              *=================================================*
      *              * Funzione "ST" : Stop                            *
      *              *-------------------------------------------------*
      *                     *------------------------------------------*
      *                     * Set 'stop' eseguito                      *
      *                     *------------------------------------------*
           move      high-value           to   v-q99                  .
           go to     qry-999.
       qry-040.
      *              *=================================================*
      *              * Funzione "MK" : Mark                            *
      *              *-------------------------------------------------*
      *                     *------------------------------------------*
      *                     * Uso di w-inx/w-pnt come linea/posizione  *
      *                     *------------------------------------------*
           move      v-lnr                to   w-inx                  .
           move      v-pnr                to   w-pnt                  .
       qry-042.
      *                     *------------------------------------------*
      *                     * Bufferizzazione area di associazione     *
      *                     *------------------------------------------*
           move      v-cnt                to   f-xvi-lin(w-inx)       .
      *                     *------------------------------------------*
      *                     * Incremento numero marks nella pagina     *
      *                     *------------------------------------------*
           add       1                    to   v-qmm                  .
      *                     *------------------------------------------*
      *                     * Bufferizzazione linea di mark            *
      *                     *------------------------------------------*
           move      w-inx                to   v-qml(v-qmm)           .
      *                     *------------------------------------------*
      *                     * Bufferizzazione posizione di mark        *
      *                     *------------------------------------------*
           move      w-pnt                to   v-qmp(v-qmm)           .
           go to     qry-850.
       qry-050.
      *              *=================================================*
      *              * Funzione "WR" : Write                           *
      *              *-------------------------------------------------*
      *                     *------------------------------------------*
      *                     * Bufferizzazione linea da visualizzare    *
      *                     *------------------------------------------*
           move      v-cnt                to   b-lin(v-lnr)           .
      *                     *------------------------------------------*
      *                     * Incremento numero linea                  *
      *                     *------------------------------------------*
           add       1                    to   v-lnr                  .
           go to     qry-850.
       qry-060.
      *              *=================================================*
      *              * Funzione "LF" : Line-Feed                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero linea                     *
      *                  *---------------------------------------------*
           add       1                    to   v-lnr                  .
       qry-070.
      *              *=================================================*
      *              * Funzione "VP" : Vertical Positioning            *
      *              *-------------------------------------------------*
           go to     qry-900.
       qry-080.
      *              *=================================================*
      *              * Funzione "GL" : Get Line                        *
      *              *-------------------------------------------------*
           move      b-lin(v-lin)         to   v-alf                  .
           go to     qry-999.
       qry-090.
      *              *=================================================*
      *              * Funzione "LS" : Line-Number Save                *
      *              *-------------------------------------------------*
           if        v-qlp                <    08
                     add    1             to   v-qlp
                     move   v-lnr         to   v-qll(v-qlp)           .
           go to     qry-999.
       qry-100.
      *              *=================================================*
      *              * Funzione "LR" : Line-Number Restore             *
      *              *-------------------------------------------------*
           if        v-qlp                >    zero
                     subtract 1           from v-qlp
                     move     v-qll(v-qlp)
                                          to   v-lnr                  .
           go to     qry-900.
       qry-150.
      *              *=================================================*
      *              * Funzione "PF" : Print Field                     *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
           add       1
                     v-edl              giving w-inx                  .
           move      high-value           to   v-edx(w-inx)           .
           move      v-pos                to   w-pnt                  .
           string    v-edt      delimited by   high-value
                                          into b-lin(v-lin)
                                  with pointer w-pnt                  .
           move      spaces               to   v-edx(w-inx)           .
           move      "PF"                 to   v-ope                  .
           move      zero                 to   w-inx                  .
           inspect   v-edm            tallying w-inx
                     for    all "+"   replacing
                            all "+"       by   spaces                 .
           if        w-inx                =    zero
                     go to  qry-999
           else      move   v-lin         to   w-inx
                     move   v-pos         to   w-pnt
                     go to  qry-042.
       qry-200.
      *              *=================================================*
      *              * Funzione "PA" : Page Advance                    *
      *              *-------------------------------------------------*
      *                     *------------------------------------------*
      *                     * Test se primo ingresso in Page Advance   *
      *                     *------------------------------------------*
           if        v-pag                >    zero
                     go to qry-220.
       qry-210.
      *                     *------------------------------------------*
      *                     * Se primo ingresso in Page Advance        *
      *                     *------------------------------------------*
      *                         *--------------------------------------*
      *                         * Se 'Stop' in essere si esce con Exit *
      *                         * altrimenti si esce con Spaces        *
      *                         *--------------------------------------*
           if        v-q99                =    high-value
                     go to qry-725
           else      go to qry-775.
       qry-220.
      *                     *------------------------------------------*
      *                     * Se non primo ingresso in Page Advance    *
      *                     *------------------------------------------*
      *                         *--------------------------------------*
      *                         * Se si e' in rientro dalla funzione   *
      *                         * di pre mark-point : si va' diretta-  *
      *                         * mente all'accettazione del tasto di  *
      *                         * funzione in mark-point               *
      *                         *--------------------------------------*
           if        v-qrm                not  = spaces
                     go to qry-340.
      *                         *--------------------------------------*
      *                         * Test se e' necessaria la memorizza-  *
      *                         * zione della pagina nel file di ap-   *
      *                         * poggio                               *
      *                         *--------------------------------------*
      *                             *----------------------------------*
      *                             * Se si e' in rientro dopo tratta- *
      *                             * mento di un tasto funzione non   *
      *                             * si esegue la memorizzazione del- *
      *                             * la pagina, in quanto la pagina   *
      *                             * attualmente visualizzata e' pari *
      *                             * alla pagina da cui si e' usciti  *
      *                             * momentaneamente per eseguire la  *
      *                             * subroutine utente di trattamento *
      *                             *----------------------------------*
           if        v-qpn                not  = v-pag or
                     v-qrp                not  = zero
                     go to qry-250.
      *                         *--------------------------------------*
      *                         * Memorizzazione della pagina video    *
      *                         * nel file di appoggio                 *
      *                         *--------------------------------------*
      *                            *-----------------------------------*
      *                            * Incremento del max record number  *
      *                            * utilizzato per il file di appgg-  *
      *                            * gio di due unita', la prima per   *
      *                            * i mark-point, la seconda per l'-  *
      *                            * immagine vera e propria           *
      *                            *-----------------------------------*
           add       2                    to   z-rcn                  .
      *                            *-----------------------------------*
      *                            * Scrittura record associato ai     *
      *                            * mark-points della pagina          *
      *                            *-----------------------------------*
      *                                *-------------------------------*
      *                                * Preparazione record number    *
      *                                *-------------------------------*
           subtract  1                    from z-rcn
                                        giving f-xvi-key              .
      *                                *-------------------------------*
      *                                * Completamento del record, che *
      *                                * e' gia' stato riempito duran- *
      *                                * te l'esecuzione dell'interro- *
      *                                * gazione mediante le funzioni  *
      *                                * attinenti i mark-points, con  *
      *                                * le informazioni indicanti il  *
      *                                * numero di mark-points mante-  *
      *                                * nute in aree a parte          *
      *                                *-------------------------------*
           move      v-q23                to   f-xvi-lin(23)          .
           move      v-q24                to   f-xvi-lin(24)          .
      *                                *-------------------------------*
      *                                * Scrittura effettiva del re-   *
      *                                * cord su disco                 *
      *                                *-------------------------------*
           perform   imw-000              thru imw-999                .
      *                            *-----------------------------------*
      *                            * Scrittura record associato all' - *
      *                            * immagine vera e propria della pa- *
      *                            * gina                              *
      *                            *-----------------------------------*
      *                                *-------------------------------*
      *                                * Preparazione record number    *
      *                                *-------------------------------*
           move      z-rcn                to   f-xvi-key              .
      *                                *-------------------------------*
      *                                * Preparazione del record con   *
      *                                * l'immagine della pagina       *
      *                                *-------------------------------*
           move      b                    to   f-xvi-dat              .
      *                                *-------------------------------*
      *                                * Scrittura effettiva del re-   *
      *                                * cord su disco                 *
      *                                *-------------------------------*
           perform   imw-000              thru imw-999                .
      *                            *-----------------------------------*
      *                            * Distruzione buffer originale per  *
      *                            * impedire che le linee siano con-  *
      *                            * siderate pari alle precedenti     *
      *                            *-----------------------------------*
           move      high-values          to   b                      .
       qry-250.
      *                         *--------------------------------------*
      *                         * Visualizzazione e trattamento della  *
      *                         * pagina : controlli preliminari       *
      *                         *--------------------------------------*
      *                             *----------------------------------*
      *                             * Test se il trattamento pagina    *
      *                             * deve essere eseguito             *
      *                             *----------------------------------*
      *                                 *------------------------------*
      *                                 * Test su tipo funzionamento,  *
      *                                 * se automatico o no           *
      *                                 *------------------------------*
           if        v-qau                =    "0"
                     go to qry-260
           else if   v-qau                =    "5"
                     go to qry-280
           else      go to qry-270.
       qry-260.
      *                                 *------------------------------*
      *                                 * Se funzionamento manuale     *
      *                                 *------------------------------*
      *                                     *--------------------------*
      *                                     * A visualizzazione pagina *
      *                                     * vera e propria           *
      *                                     *--------------------------*
           go to     qry-300.
       qry-270.
      *                                 *------------------------------*
      *                                 * Se funzionamento automatico  *
      *                                 *------------------------------*
      *                                     *--------------------------*
      *                                     * Se non si e' ancora rag- *
      *                                     * giunto lo 'Stop' non si  *
      *                                     * esegue la visualizzazio- *
      *                                     * ne e si prosegue con u-  *
      *                                     * scita per Spaces         *
      *                                     *--------------------------*
           if        v-q99                =    low-value
                     go to qry-775.
      *                                     *--------------------------*
      *                                     * Se invece lo 'Stop' e'   *
      *                                     * gia' stato raggiunto     *
      *                                     *--------------------------*
      *                                         *----------------------*
      *                                         * Forzatura del tipo   *
      *                                         * funzionamento in ma- *
      *                                         * nuale                *
      *                                         *----------------------*
           move      "0"                  to   v-qau                  .
      *                                         *----------------------*
      *                                         * Forzatura del numero *
      *                                         * pagina da visualiz-  *
      *                                         * zare e trattare a 1  *
      *                                         *----------------------*
           move      1                    to   v-qpn                  .
      *                                         *----------------------*
      *                                         * A visualizzazione e  *
      *                                         * trattamento pagina   *
      *                                         *----------------------*
           go to     qry-300.
       qry-280.
      *                                 *------------------------------*
      *                                 * Se funzionamento sotto Tab   *
      *                                 *------------------------------*
      *                                     *--------------------------*
      *                                     * Se non si e' ancora rag- *
      *                                     * giunto lo 'Stop' non si  *
      *                                     * esegue la visualizzazio- *
      *                                     * ne e si prosegue con u-  *
      *                                     * scita per Spaces         *
      *                                     *--------------------------*
           if        v-q99                =    low-value
                     go to qry-775.
      *                                     *--------------------------*
      *                                     * Se invece lo 'Stop' e'   *
      *                                     * gia' stato raggiunto     *
      *                                     *--------------------------*
      *                                         *----------------------*
      *                                         * Forzatura del tipo   *
      *                                         * funzionamento in ma- *
      *                                         * nuale                *
      *                                         *----------------------*
           move      "0"                  to   v-qau                  .
      *                                         *----------------------*
      *                                         * A visualizzazione e  *
      *                                         * trattamento pagina   *
      *                                         *----------------------*
           go to     qry-300.
       qry-300.
      *                         *--------------------------------------*
      *                         * Visualizzazione e trattamento della  *
      *                         * pagina 'v-qpn' : esecuzione          *
      *                         *--------------------------------------*
      *                            *-----------------------------------*
      *                            * Ripresa pagina da file appoggio   *
      *                            *-----------------------------------*
      *                                *-------------------------------*
      *                                * Determinazione record number  *
      *                                * per record immagine           *
      *                                *-------------------------------*
           multiply  v-qpn                by   2
                                        giving f-xvi-key              .
           add       v-qsr                to   f-xvi-key              .
      *                                *-------------------------------*
      *                                * Lettura record per l'immagine *
      *                                *-------------------------------*
           perform   imr-000              thru imr-999                .
      *                            *-----------------------------------*
      *                            * Forzatura della linea 22 al valo- *
      *                            * re standard per questa linea      *
      *                            *-----------------------------------*
           move      all    "="           to   f-xvi-lin (22)         .
      *                            *-----------------------------------*
      *                            * Forzatura della linea 23 a spaces *
      *                            *-----------------------------------*
           move      spaces               to   f-xvi-lin (23)         .
      *                            *-----------------------------------*
      *                            * Preparazione della linea 24 con   *
      *                            * l'indicazione della pagina        *
      *                            *-----------------------------------*
      *                                *-------------------------------*
      *                                * Editing numero pagina         *
      *                                *-------------------------------*
           move      spaces               to   w-npg-num              .
           if        v-qpn                <    10
                     move  v-qpn          to   w-npg-001
           else if   v-qpn                <    100
                     move  v-qpn          to   w-npg-002
           else if   v-qpn                <    1000
                     move  v-qpn          to   w-npg-003
           else if   v-qpn                <    10000
                     move  v-qpn          to   w-npg-004
           else      move  v-qpn          to   w-npg-005              .
      *                                *-------------------------------*
      *                                * Editing dicitura di [Fine]    *
      *                                *-------------------------------*
           if        v-qpn                =    v-pag
                     if    v-q99          =    low-value
                           move spaces    to   w-npg-fin
                     else  move "[Fine]"  to   w-npg-fin
           else      move  spaces         to   w-npg-fin              .
      *                                *-------------------------------*
      *                                * Composizione linea 24         *
      *                                *-------------------------------*
           move      spaces               to   f-xvi-lin (24)         .
           string    w-npg-lit
                                delimited by   size
                     w-npg-num  delimited by   spaces
                     " "        delimited by   size
                     w-npg-fin  delimited by   size
                                          into f-xvi-lin (24)         .
      *                            *-----------------------------------*
      *                            * Visualizzazione pagina mediante   *
      *                            * richiamo della funzione di Resto- *
      *                            * re immagine video                 *
      *                            *-----------------------------------*
           perform   rsm-000              thru rsm-999                .
      *                            *-----------------------------------*
      *                            * Flag di prompt per accettazione   *
      *                            * funzione visibile : Off           *
      *                            *-----------------------------------*
           move      spaces               to   w-mkf                  .
      *                            *-----------------------------------*
      *                            * Ripresa record associato ai mark- *
      *                            * points per la pagina              *
      *                            *-----------------------------------*
      *                                *-------------------------------*
      *                                * Determinazione record number  *
      *                                * per record mark-points        *
      *                                *-------------------------------*
           subtract  1                    from f-xvi-key              .
      *                                *-------------------------------*
      *                                * Lettura record associato ai   *
      *                                * mark-points                   *
      *                                *-------------------------------*
           perform   iml-000              thru iml-999                .
      *                                *-------------------------------*
      *                                * Informazioni supplementari    *
      *                                * associate ai mark-points in   *
      *                                * area di lavoro di origine     *
      *                                *-------------------------------*
           move      f-xvi-lin(23)        to   v-q23                  .
           move      f-xvi-lin(24)        to   v-q24                  .
       qry-330.
      *                            *-----------------------------------*
      *                            * Accettazione funzione             *
      *                            *-----------------------------------*
      *                                *-------------------------------*
      *                                * Determinazione flags per i    *
      *                                * tasti funzione ammissibili    *
      *                                *-------------------------------*
      *                                    *---------------------------*
      *                                    * Nxsc                      *
      *                                    *---------------------------*
           if        v-qpn                =    v-pag  and
                     v-q99                =    high-value
                     move  spaces         to   w-nxt
           else      move  "#"            to   w-nxt                  .
      *                                    *---------------------------*
      *                                    * Prsc                      *
      *                                    *---------------------------*
           if        v-qpn                >    1
                     move  "#"            to   w-prv
           else      move  spaces         to   w-prv                  .
      *                                    *---------------------------*
      *                                    * Back                      *
      *                                    *---------------------------*
           if        v-qpn                >    1
                     move  "#"            to   w-bck
           else      move  spaces         to   w-bck                  .
      *                                    *---------------------------*
      *                                    * Tab                       *
      *                                    *---------------------------*
           if        v-qpn                =    v-pag and
                     v-q99                =    high-value
                     move  spaces         to   w-tab
           else      move  "#"            to   w-tab                  .
       qry-340.
      *                                *-------------------------------*
      *                                * Interazione con l'utente      *
      *                                *-------------------------------*
      *                                    *---------------------------*
      *                                    * Se esiste un mark-point   *
      *                                    * di rientro, ci si posi-   *
      *                                    * ziona a quel mark-point   *
      *                                    *---------------------------*
           if        v-qrp                not  = zero
                     move   v-qrp         to   w-pnt
                     move   zero          to   v-qrp
                     go to  qry-400.
      *                                    *---------------------------*
      *                                    * Altrimenti si trova il    *
      *                                    * primo mark-point, per po- *
      *                                    * zionarsi ad esso, ma se   *
      *                                    * non ne esiste alcuno per  *
      *                                    * la pagina si va' all'ac-  *
      *                                    * cettazione funzione a     *
      *                                    * piede video               *
      *                                    *---------------------------*
      *                                        *-----------------------*
      *                                        * Numero mark-point at- *
      *                                        * tuale a zero          *
      *                                        *-----------------------*
           move      zero                 to   w-pnt                  .
       qry-350.
      *                                        *-----------------------*
      *                                        * Incremento numero di  *
      *                                        * mark-point, e se ol-  *
      *                                        * tre il numero massimo *
      *                                        * di mark-points memo-  *
      *                                        * rizzati per la pagina *
      *                                        * si va' ad accettazio- *
      *                                        * ne funzione a piede   *
      *                                        * video                 *
      *                                        *-----------------------*
           add       1                    to   w-pnt                  .
           if        w-pnt                >    v-qmm
                     go to qry-500.
       qry-400.
      *                                    *---------------------------*
      *                                    * Accettazione funzione in  *
      *                                    * mark-point                *
      *                                    *---------------------------*
      *                                        *-----------------------*
      *                                        * Eventuale cancella-   *
      *                                        * zione del prompt per  *
      *                                        * l'accettazione della  *
      *                                        * funzione a linea 24   *
      *                                        *                       *
      *                                        * Differenziata a se-   *
      *                                        * conda se modo a 80 o  *
      *                                        * 132 colonne           *
      *                                        *-----------------------*
           if        w-mkf                =    spaces
                     go to qry-410.
           move      spaces               to   w-mkf                  .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      z-mmx                to   v-pos                  .
           subtract  3                    from v-pos                  .
           move      spaces               to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       qry-410.
      *                                        *-----------------------*
      *                                        * Salvataggio linea e   *
      *                                        * posizione mark-point  *
      *                                        *-----------------------*
           move      v-qml (w-pnt)        to   w-svl                  .
           move      v-qmp (w-pnt)        to   w-svp                  .
       qry-450.
      *                                        *-----------------------*
      *                                        * Accettazione in mark- *
      *                                        * point                 *
      *                                        *-----------------------*
      *                                            *-------------------*
      *                                            * Se si e' in rien- *
      *                                            * tro dalla funzio- *
      *                                            * ne mark-point si  *
      *                                            * va' sicuramente   *
      *                                            * all'accettazione  *
      *                                            * dopo aver rimesso *
      *                                            * in Off il segnale *
      *                                            * di rientro        *
      *                                            *-------------------*
           if        v-qrm                not  = spaces
                     move  spaces         to   v-qrm
                     go to qry-455.
      *                                            *-------------------*
      *                                            * Se abilitazione   *
      *                                            * pre mark-point    *
      *                                            * attiva e non si   *
      *                                            * e' in rientro da  *
      *                                            * pre mark-point :  *
      *                                            * si prepara il pre *
      *                                            * mark-point e poi  *
      *                                            * si va all'uscita  *
      *                                            *-------------------*
           if        v-qpm                not  = spaces
                     move  "S"            to   v-qrm
                     move  "-PM-"         to   v-key
                     go to qry-470.
       qry-455.
      *                                            *-------------------*
      *                                            * Parametri genera- *
      *                                            * li                *
      *                                            *-------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      w-svl                to   v-lin                  .
           move      w-svp                to   v-pos                  .
      *                                            *-------------------*
      *                                            * Preparazione del- *
      *                                            * le function keys  *
      *                                            *-------------------*
      *                                                *---------------*
      *                                                * Function keys *
      *                                                * preparate dal *
      *                                                * programma che *
      *                                                * ha chiamato   *
      *                                                *---------------*
           move      v-qsu                to   v-ufk                  .
      *                                                *---------------*
      *                                                * Determinazio- *
      *                                                * ne se tasto   *
      *                                                * 'Slct' ammis- *
      *                                                * sibile        *
      *                                                *---------------*
           if        v-pfk (01)           =    "SLCT" or
                     v-pfk (02)           =    "SLCT" or
                     v-pfk (03)           =    "SLCT" or
                     v-pfk (04)           =    "SLCT" or
                     v-pfk (05)           =    "SLCT" or
                     v-pfk (06)           =    "SLCT" or
                     v-pfk (07)           =    "SLCT" or
                     v-pfk (08)           =    "SLCT" or
                     v-pfk (09)           =    "SLCT" or
                     v-pfk (10)           =    "SLCT"
                     move  "#"            to   w-slc
           else      move  spaces         to   w-slc                  .
      *                                                *---------------*
      *                                                * Fisse         *
      *                                                *---------------*
      *                                                    *-----------*
      *                                                    * Up        *
      *                                                    *-----------*
           if        w-pnt                >    1   or
                     w-prv                =    "#"
                     move   "UP  "        to   v-pfk (11)             .
      *                                                    *-----------*
      *                                                    * Down      *
      *                                                    *-----------*
           move      "DOWN"               to   v-pfk (12)             .
      *                                                    *-----------*
      *                                                    * Nxsc      *
      *                                                    *-----------*
           if        w-nxt                =    "#"
                     move   "NXSC"        to   v-pfk (13)             .
      *                                                    *-----------*
      *                                                    * Prsc      *
      *                                                    *-----------*
           if        w-prv                =    "#"
                     move   "PRSC"        to   v-pfk (14)             .
      *                                                    *-----------*
      *                                                    * Back      *
      *                                                    *-----------*
           if        w-bck                =    "#"
                     move   "BACK"        to   v-pfk (15)             .
      *                                                    *-----------*
      *                                                    * Tab       *
      *                                                    *-----------*
           if        w-tab                =    "#"
                     move   "TAB "        to   v-pfk (16)             .
      *                                                    *-----------*
      *                                                    * Exit      *
      *                                                    *-----------*
           move      "EXIT"               to   v-pfk (20)             .
      *                                            *-------------------*
      *                                            * Accettazione      *
      *                                            *-------------------*
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
      *                                        *-----------------------*
      *                                        * Normalizzazione del   *
      *                                        * tasto funzione : se   *
      *                                        * l'utente ha digitato  *
      *                                        * Return si ritorna al  *
      *                                        * chiamante :           *
      *                                        * - 'Slct' se il tasto  *
      *                                        *          funzione era *
      *                                        *          ammesso      *
      *                                        * - Si ritorna all'ac-  *
      *                                        *          cettazione   *
      *                                        *          Mark-Point   *
      *                                        *          se il tasto  *
      *                                        *          funzione non *
      *                                        *          era ammesso  *
      *                                        *-----------------------*
           if        v-key                =    spaces
                     if    w-slc          =    spaces
                           go to qry-450
                     else  move  "SLCT"   to   v-key                  .
      *                                        *-----------------------*
      *                                        * Deviazione a seconda  *
      *                                        * del tasto funzione    *
      *                                        *-----------------------*
      *                                            *-------------------*
      *                                            * Se Exit : uscita  *
      *                                            *-------------------*
           if        v-key                =    "EXIT"
                     go to qry-725.
      *                                            *-------------------*
      *                                            * Se Up : regres-   *
      *                                            * sione mark-point  *
      *                                            * a meno che non si *
      *                                            * sia sul primo,    *
      *                                            * caso in cui si    *
      *                                            * simula, se possi- *
      *                                            * bile, un Prsc     *
      *                                            *-------------------*
           if        v-key                =    "UP  "
                     if      w-pnt        >    1
                             subtract 1   from w-pnt
                             go to    qry-400
                     else if w-prv        =    "#"
                             move  "PRSC" to   v-key
                     else    subtract 1   from w-pnt
                             go to    qry-400.
      *                                            *-------------------*
      *                                            * Se Down : progres-*
      *                                            * sione mark-point  *
      *                                            * a meno che non si *
      *                                            * sia sull'ultimo,  *
      *                                            * caso in cui si    *
      *                                            * simula, se possi- *
      *                                            * bile, un Nxsc     *
      *                                            *-------------------*
           if        v-key                =    "DOWN"
                     if      w-pnt        <    v-qmm
                             go to qry-350
                     else if w-nxt        =    "#"
                             move  "NXSC" to   v-key
                     else    go to qry-350.
      *                                            *-------------------*
      *                                            * Se Prsc : decre-  *
      *                                            * mento numero pa-  *
      *                                            * gina e aggancio a *
      *                                            * visualizzazione   *
      *                                            * pagina            *
      *                                            *-------------------*
           if        v-key                =    "PRSC"
                     subtract 1           from v-qpn
                     go to qry-250.
      *                                            *-------------------*
      *                                            * Se Nxsc : se con  *
      *                                            * cio' si raggiunge *
      *                                            * una pagina gia'   *
      *                                            * memorizzata, si   *
      *                                            * incrementa il nu- *
      *                                            * mero pagina e si  *
      *                                            * va' a visualizza- *
      *                                            * zione pagina, al- *
      *                                            * trimenti si esce  *
      *                                            *-------------------*
           if        v-key                =    "NXSC"
                     if     v-qpn         <    v-pag
                            add    1      to   v-qpn
                            go to  qry-250
                     else   go to  qry-750.
      *                                            *-------------------*
      *                                            * Se Back : forza-  *
      *                                            * tura numero pagi- *
      *                                            * na 1 e aggancio a *
      *                                            * visualizzazione   *
      *                                            * pagina            *
      *                                            *-------------------*
           if        v-key                =    "BACK"
                     move  1              to   v-qpn
                     go to qry-250.
      *                                            *-------------------*
      *                                            * Se Tab : a trat-  *
      *                                            * tamento Tab       *
      *                                            *-------------------*
           if        v-key                =    "TAB "
                     go to qry-600.
       qry-470.
      *                                            *-------------------*
      *                                            * Se function-key   *
      *                                            * utente            *
      *                                            *-------------------*
      *                                                *---------------*
      *                                                * Salvataggio   *
      *                                                * mark-point in *
      *                                                * cui rientrare *
      *                                                *---------------*
           move      w-pnt                to   v-qrp                  .
      *                                                *---------------*
      *                                                * Preparazione  *
      *                                                * linea per il  *
      *                                                * mark-point    *
      *                                                *---------------*
           move      w-svl                to   v-lin                  .
      *                                                *---------------*
      *                                                * Preparazione  *
      *                                                * posizione per *
      *                                                * il mark-point *
      *                                                *---------------*
           move      w-svp                to   v-pos                  .
      *                                                *---------------*
      *                                                * Preparazione  *
      *                                                * immagine li-  *
      *                                                * nea relativa  *
      *                                                * al mark-point *
      *                                                *---------------*
           move      f-xvi-lin(w-svl)     to   v-cnt                  .
      *                                                *---------------*
      *                                                * Uscita        *
      *                                                *---------------*
           go to     qry-999.
       qry-500.

      *                                    *---------------------------*
      *                                    * Accettazione funzione a   *
      *                                    * piede video a linea 24    *
      *                                    *---------------------------*
      *                                        *-----------------------*
      *                                        * Eventuale visualizza- *
      *                                        * zione del prompt per  *
      *                                        * l'accettazione della  *
      *                                        * funzione a linea 24   *
      *                                        *                       *
      *                                        * Differenziata a se-   *
      *                                        * conda se modo a 80 o  *
      *                                        * 132 colonne           *
      *                                        *-----------------------*
           if        w-mkf                not  = spaces
                     go to qry-550.
           move      "#"                  to   w-mkf                  .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      z-mmx                to   v-pos                  .
           subtract  3                    from v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
       qry-550.
      *                                        *-----------------------*
      *                                        * Accettazione a piede  *
      *                                        *                       *
      *                                        * Differenziata a se-   *
      *                                        * conda se modo a 80 o  *
      *                                        * 132 colonne           *
      *                                        * video                 *
      *                                        *-----------------------*
      *                                            *-------------------*
      *                                            * Parametri genera- *
      *                                            * li                *
      *                                            *-------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      24                   to   v-lin                  .
           move      z-mmx                to   v-pos                  .
           subtract  2                    from v-pos                  .
      *                                            *-------------------*
      *                                            * Preparazione del- *
      *                                            * le function keys  *
      *                                            *-------------------*
      *                                                *---------------*
      *                                                * Normalizza-   *
      *                                                * zione preli-  *
      *                                                * minare        *
      *                                                *---------------*
           move      spaces               to   v-ufk                  .
      *                                                *---------------*
      *                                                * Preparazione  *
      *                                                *---------------*
      *                                                    *-----------*
      *                                                    * Up        *
      *                                                    *-----------*
           if        v-qmm                >    zero
                     move   "UP  "        to   v-pfk (11)             .
      *                                                    *-----------*
      *                                                    * Nxsc      *
      *                                                    *-----------*
           if        w-nxt                =    "#"
                     move   "NXSC"        to   v-pfk (13)             .
      *                                                    *-----------*
      *                                                    * Prsc      *
      *                                                    *-----------*
           if        w-prv                =    "#"
                     move   "PRSC"        to   v-pfk (14)             .
      *                                                    *-----------*
      *                                                    * Back      *
      *                                                    *-----------*
           if        w-bck                =    "#"
                     move   "BACK"        to   v-pfk (15)             .
      *                                                    *-----------*
      *                                                    * Tab       *
      *                                                    *-----------*
           if        w-tab                =    "#"
                     move   "TAB "        to   v-pfk (16)             .
      *                                                    *-----------*
      *                                                    * Exit      *
      *                                                    *-----------*
           move      "EXIT"               to   v-pfk (20)             .
      *                                            *-------------------*
      *                                            * Accettazione      *
      *                                            *-------------------*
           call      d                   using z
                                               b
                                               v
                                               f-xvi                  .
      *                                        *-----------------------*
      *                                        * Deviazione a seconda  *
      *                                        * del tasto funzione    *
      *                                        *-----------------------*
      *                                            *-------------------*
      *                                            * Se Exit : uscita  *
      *                                            *-------------------*
           if        v-key                =    "EXIT"
                     go to qry-725.
      *                                            *-------------------*
      *                                            * Se Up : ad ultimo *
      *                                            * mark-point        *
      *                                            *-------------------*
           if        v-key                =    "UP  "
                     move  v-qmm          to   w-pnt
                     go to qry-400.
      *                                            *-------------------*
      *                                            * Se Prsc : decre-  *
      *                                            * mento numero pa-  *
      *                                            * gina e aggancio a *
      *                                            * visualizzazione   *
      *                                            * pagina            *
      *                                            *-------------------*
           if        v-key                =    "PRSC"
                     subtract 1           from v-qpn
                     go to qry-250.
      *                                            *-------------------*
      *                                            * Se Nxsc : se con  *
      *                                            * cio' si raggiunge *
      *                                            * una pagina gia'   *
      *                                            * memorizzata, si   *
      *                                            * incrementa il nu- *
      *                                            * mero pagina e si  *
      *                                            * va' a visualizza- *
      *                                            * zione pagina, al- *
      *                                            * trimenti si esce  *
      *                                            *-------------------*
           if        v-key                =    "NXSC"
                     if     v-qpn         <    v-pag
                            add    1      to   v-qpn
                            go to  qry-250
                     else   go to  qry-750.
      *                                            *-------------------*
      *                                            * Se Back : forza-  *
      *                                            * tura numero pagi- *
      *                                            * na 1 e aggancio a *
      *                                            * visualizzazione   *
      *                                            * pagina            *
      *                                            *-------------------*
           if        v-key                =    "BACK"
                     move  1              to   v-qpn
                     go to qry-250.
      *                                            *-------------------*
      *                                            * Se Tab            *
      *                                            *-------------------*
           if        v-key                =    "TAB "
                     go to qry-600.
       qry-570.
      *                                            *-------------------*
      *                                            * Se tasto funzione *
      *                                            * non riconosciuto  *
      *                                            *-------------------*
      *                                                *---------------*
      *                                                * Rientro ad    *
      *                                                * accettazione  *
      *                                                * a piede video *
      *                                                *---------------*
            go to    qry-550.
       qry-600.
      *                     *------------------------------------------*
      *                     * Trattamento del tasto Tab                *
      *                     *------------------------------------------*
      *                         *--------------------------------------*
      *                         * Forzatura del modo funzionamento Tab *
      *                         *--------------------------------------*
           move      "5"                  to   v-qau                  .
      *                         *--------------------------------------*
      *                         * Test se si e' gia' raggiunto lo sta- *
      *                         * to di 'Stop' oppure no               *
      *                         *--------------------------------------*
           if        v-q99                =    low-value
                     go to qry-620.
       qry-610.
      *                         *--------------------------------------*
      *                         * Se si e' gia' raggiunto lo 'Stop'    *
      *                         *--------------------------------------*
      *                             *----------------------------------*
      *                             * Forzatura del modo funzionamento *
      *                             * manuale                          *
      *                             *----------------------------------*
           move      "0"                  to   v-qau                  .
      *                             *----------------------------------*
      *                             * Forzatura ultima pagina          *
      *                             *----------------------------------*
           move      v-pag                to   v-qpn                  .
      *                             *----------------------------------*
      *                             * A visualizzazione ultima pagina  *
      *                             *----------------------------------*
           go to     qry-250.
       qry-620.
      *                         *--------------------------------------*
      *                         * Se lo 'Stop' non e' ancora stato     *
      *                         * raggiunto                            *
      *                         *--------------------------------------*
      *                             *----------------------------------*
      *                             * Uscita per Spaces                *
      *                             *----------------------------------*
           go to     qry-750.
       qry-725.
      *                     *------------------------------------------*
      *                     * Uscita con Exit da Page Advance          *
      *                     *------------------------------------------*
           move      "EXIT"               to   v-key                  .
           go to     qry-999.
       qry-750.
      *                     *------------------------------------------*
      *                     * Uscita per pagina successiva             *
      *                     *------------------------------------------*
      *                         *--------------------------------------*
      *                         * Se il segnale di 'Stop' indica che   *
      *                         * questo e' gia' avvenuto si esce im-  *
      *                         * mediatamente                         *
      *                         *--------------------------------------*
           if        v-q99                =    high-value
                     go to qry-999.
       qry-775.
      *                     *------------------------------------------*
      *                     * Preparazioni per pagina successiva       *
      *                     *------------------------------------------*
      *                         *--------------------------------------*
      *                         * Uscita per Spaces                    *
      *                         *--------------------------------------*
           move      spaces               to   v-key                  .
      *                         *--------------------------------------*
      *                         * Record file appoggio, per ospitare   *
      *                         * i mark-points, a spaces              *
      *                         *--------------------------------------*
           move      spaces               to   f-xvi-dat              .
      *                         *--------------------------------------*
      *                         * Buffer immagine a spaces             *
      *                         *--------------------------------------*
           move      spaces               to   b                      .
      *                         *--------------------------------------*
      *                         * Mark-point cui rientrare : zero      *
      *                         *--------------------------------------*
           move      zero                 to   v-qrp                  .
      *                         *--------------------------------------*
      *                         * Numero di mark-point nella pagina :  *
      *                         * zero                                 *
      *                         *--------------------------------------*
           move      zero                 to   v-qmm                  .
      *                         *--------------------------------------*
      *                         * Incremento numero pagina attuale     *
      *                         *--------------------------------------*
           add       1                    to   v-pag                  .
      *                         *--------------------------------------*
      *                         * Numero pagina cui rientrare pari al  *
      *                         * numero pagina attuale incrementato   *
      *                         *--------------------------------------*
           move      v-pag                to   v-qpn                  .
       qry-800.
      *                         *--------------------------------------*
      *                         * Inizializzazione line-number         *
      *                         *--------------------------------------*
           move      1                    to   v-lnr                  .
      *                        *---------------------------------------*
      *                        * Livello profondita' per funzione di   *
      *                        * line-number save : zero               *
      *                        *---------------------------------------*
           move      zero                 to   v-qlp                  .
       qry-850.
      *                        *---------------------------------------*
      *                        * Immagine linea a spaces               *
      *                        *---------------------------------------*
           move      spaces               to   v-cnt                  .
       qry-900.
      *                        *---------------------------------------*
      *                        * Determinazione numero linee residue   *
      *                        *---------------------------------------*
           subtract  v-lnr                from 22
                                        giving v-res                  .
      *                        *---------------------------------------*
      *                        * Uscita                                *
      *                        *---------------------------------------*
           go to     qry-999.
       qry-999.
           exit.
