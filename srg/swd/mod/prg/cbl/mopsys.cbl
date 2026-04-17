       Identification Division.
       Program-Id.                                 mopsys             .
      *================================================================*
      *                                                                *
      * Modulo di interfacciamento con sistema operativo ospite        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Qusto modulo, che utilizza l'interfaccia 'o', provvede all'in- *
      * terfacciamento con il sistema operativo ospite, allo scopo di  *
      * eliminarne le idiosincrasie rispetto al sistema operativo di   *
      * base UNIX.                                                     *
      *                                                                *
      * Si presume che i richiami a questo modulo siano eseguiti pre-  *
      * sumendo il sistema operativo Unix.                             *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * OP          Open modulo                                        *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "OP"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita                  *
      *                                                                *
      *                              - Spaces : Ok                     *
      *                              - SX     : Sistema operativo      *
      *                                         ospite non ricono-     *
      *                                         sciuto                 *
      *                                                                *
      *                      o-sys = Sigla del sistema operativo ospi- *
      *                              te                                *
      *                                                                *
      *                              - Spaces : Ok                     *
      *                              - SX     : Sistema operativo      *
      *                                         ospite non ricono-     *
      *                                         sciuto. In questo      *
      *                                         caso il valore o-oss   *
      *                                         indica la sigla del    *
      *                                         sistema operativo      *
      *                                         ospite.                *
      *                                                                *
      *                      o-osc = Codice del sistema operativo      *
      *                                                                *
      *                      o-oss = Sigla del sistema operativo       *
      *                                                                *
      *                      o-osd = Descrizione del sistema operativo *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * O2          Completamento open modulo                          *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "O2"                              *
      *                                                                *
      *                      o-com = Tipo di completamento             *
      *                               - HOME : V_DKB_HOME              *
      *                               - USER : V_DKB_USER              *
      *                               - TTYC : V_DKB_TTYC              *
      *                               - TERM : V_DKB_TERM              *
      *                               - RUNT : V_DKB_RUNT              *
      *                               - PFIX : V_DKB_GETP, componente  *
      *                                               relativa a PFIX  *
      *                               - HOID : V_DKB_GETP, componente  *
      *                                               relativa a HOID  *
      *                               - SUBT : V_DKB_SUBT              *
      *                               - AZIE : V_DKB_AZIE              *
      *                               - UTEN : V_DKB_UTEN              *
      *                               - PRMS : V_DKB_PRMS              *
      *                               - POST : V_DKB_POST              *
      *                                                                *
      *                      o-pat = Valore della variabile di envi-   *
      *                              ronment passata                   *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * I2          Richiesta valori parametri di completamento open   *
      *             modulo                                             *
      *                                                                *
      *             Input  : o-ope = "I2"                              *
      *                                                                *
      *                      o-com = Tipo di parametro di completamen- *
      *                              to                                *
      *                               - HOME : V_DKB_HOME              *
      *                               - USER : V_DKB_USER              *
      *                               - TTYC : V_DKB_TTYC              *
      *                               - TERM : V_DKB_TERM              *
      *                               - RUNT : V_DKB_RUNT              *
      *                               - PFIX : V_DKB_GETP, componente  *
      *                                               relativa a PFIX  *
      *                               - HOID : V_DKB_GETP, componente  *
      *                                               relativa a HOID  *
      *                               - SUBT : V_DKB_SUBT              *
      *                               - AZIE : V_DKB_AZIE              *
      *                               - UTEN : V_DKB_UTEN              *
      *                               - PRMS : V_DKB_PRMS              *
      *                               - POST : V_DKB_POST              *
      *                                                                *
      *                                                                *
      *             Output : o-pat = Valore della variabile di envi-   *
      *                              ronment passata con l'operazio-   *
      *                              ne "O2"                           *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * CL          Close modulo                                       *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "CL"                              *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * AZ          Dichiarazione codice azienda                       *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "AZ"                              *
      *                                                                *
      *                      o-com = Sigla azienda                     *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * UT          Dichiarazione codice utente                        *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "UT"                              *
      *                                                                *
      *                      o-com = Sigla utente                      *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * A0          Inizializzazione composizione pathname completo di *
      *             file                                               *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "A0"                              *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * A5          Comunicazione componente pathname di file          *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "A5"                              *
      *                                                                *
      *                      o-com = componente                        *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *                                                                *
      *             Note   : Presume il preventivo richiamo di A0      *
      *                                                                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * A9          Richiesta di ottenimento del pathname completo di  *
      *             file con locazione in /abd                         *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "A9"                              *
      *                                                                *
      *                                                                *
      *             Output : o-pat = pathname                          *
      *                                                                *
      *                                                                *
      *             Note   : Presume il preventivo richiamo di A0 e di *
      *                      uno o piu' A5                             *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * C9          Richiesta di semplice concatenamento di componenti *
      *             di pathname, presumendo che il primo dei componen- *
      *             ti sia gia' stato sottoposto al filtro per /abd    *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "C9"                              *
      *                                                                *
      *                                                                *
      *             Output : o-pat = pathname                          *
      *                                                                *
      *                                                                *
      *             Note   : Presume il preventivo richiamo di A0 e di *
      *                      uno o piu' A5                             *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * R9          Richiesta di rimozione di tutti i files contenuti  *
      *             in una directory con locazione in /abd, eventual-  *
      *             mente solo quelli che hanno un certo prefisso      *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "R9"                              *
      *                                                                *
      *                      o-com = Eventuale prefisso, se a spaces   *
      *                              saranno rimossi tutti i files     *
      *                              all'interno della subdirectory    *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *                                                                *
      *             Note   : Presume il preventivo richiamo di A0 e di *
      *                      uno o piu' A5                             *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * SH          Richiesta di esecuzione di una shell interattiva   *
      *             per poi rientrare in programma                     *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "SH"                              *
      *                                                                *
      *                      o-shs = Shell script, se pari a spaces    *
      *                              si intende il richiamo di una     *
      *                              shell interattiva ad imposta-     *
      *                              zione manuale dei comandi         *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *                                                                *
      *             Note   : Non viene eseguito alcun abblencamento    *
      *                      preventivo dello schermo, ne' alcuna      *
      *                      operazione di refresh finale              *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * XB          Richiesta di esecuzione di una procedura batch     *
      *             standard, all'interno di /abd/bat                  *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "XB"                              *
      *                                                                *
      *                      o-com = Nome della procedura batch, che   *
      *                              deve trovare corrispondenza al-   *
      *                              l'interno della directory per i   *
      *                              batches /abd/bat                  *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *                                                                *
      *             Note   : Non e' ammesso che il nome della proce-   *
      *                      dura da eseguire sia seguito da parame-   *
      *                      tri per l'esecuzione.                     *
      *                                                                *
      *             Note   : Non viene eseguito alcun abblencamento    *
      *                      preventivo dello schermo, ne' alcuna      *
      *                      operazione di refresh finale              *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * X9          Richiesta di esecuzione di un comando, contenuto   *
      *             all'interno di /abd, con eventuale passaggio di    *
      *             parametri al comando.                              *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "X9"                              *
      *                                                                *
      *                      o-com = Nome del comando, che puo' corri- *
      *                              spondere ad una procedura batch,  *
      *                              e che indica l'ultimo componente  *
      *                              del pathname completo del comand- *
      *                              do stesso                         *
      *                                                                *
      *                      o-shs = Parametri che devono essere pas-  *
      *                              sati al comando oppure spaces se  *
      *                              non ci sono parametri             *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *                                                                *
      *             Note   : Presume il preventivo richiamo di A0 e di *
      *                      uno o piu' A5, per la composizione della  *
      *                      parte del pathname che precede il nome    *
      *                      stesso del comando                        *
      *                                                                *
      *             Note   : Non viene eseguito alcun abblencamento    *
      *                      preventivo dello schermo, ne' alcuna      *
      *                      operazione di refresh finale              *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * CU          Richiesta di esecuzione di una 'Call Unix', ovvero *
      *             di una chiamata al sistema diretta, senza alcun    *
      *             filtro da parte del modulo di interfaccia          *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "CU"                              *
      *                                                                *
      *                      o-shs = Chiamata al sistema               *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *                                                                *
      *             Note   : Non viene eseguito alcun abblencamento    *
      *                      preventivo dello schermo, ne' alcuna      *
      *                      operazione di refresh finale              *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * BF          Richiesta di Si/No possibile il background per il  *
      *             sistema operativo ospite                           *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "BF"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita                  *
      *                                                                *
      *                              - Spaces : Background consentito  *
      *                              - ##     : Background non ammes-  *
      *                                         so per il sistema o-   *
      *                                         perativo ospite        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * SS          Richiesta di Si/No possibile la stampa in spool    *
      *             per il sistema operativo ospite                    *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "SS"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita                  *
      *                                                                *
      *                              - Spaces : Stampa in spool con-   *
      *                                         sentita                *
      *                              - ##     : Stampa in spool non    *
      *                                         ammessa per il siste-  *
      *                                         ma operativo ospite    *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * BK          Richiesta di esecuzione in background              *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "BK"                              *
      *                                                                *
      *                      o-pat = Prefisso unico per files tempo-   *
      *                              ranei                             *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita                  *
      *                                                                *
      *                              - Spaces : Lancio dell'esecuzione *
      *                                         in background eseguito *
      *                              - ##     : Lancio dell'esecuzione *
      *                                         in background non ese- *
      *                                         guito                  *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * DA          Richiesta di esecuzione desk-accessories           *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "DA"                              *
      *                                                                *
      *                      o-com = Tipo di desk-accessory            *
      *                              ovvero variabile V_DKB_SUBT       *
      *                                                                *
      *                      o-pat = Parametri per il desk-accessory   *
      *                              ovvero variabile V_DKB_PRMS       *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita                  *
      *                                                                *
      *                              - Spaces : Desk-accessory ese-    *
      *                                         guito                  *
      *                              - ##     : Desk-accessory non     *
      *                                         eseguito               *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * LD          Richiesta del livello di esecuzione relativo ai    *
      *             desk-accessories                                   *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "LD"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita, ad indicare il  *
      *                              livello richiesto                 *
      *                                                                *
      *                              - Spaces : Esecuzione principale  *
      *                              - "00"   : Esecuzione principale  *
      *                              - "nn"   : Livello di esecuzione  *
      *                                         desk-accessory, tra i  *
      *                                         valori "01" e "99"     *
      *                                                                *
      *                      o-pat = Tipo di desk-accessory, solo se   *
      *                              livello tra "01" e "99"           *
      *                                                                *
      *                              - Spaces : Non significativo      *
      *                              - "HELP" : Help                   *
      *                              - "SHCP" : Screen-Hard-Copy       *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * LU          Richiesta tipo di licenza d'uso                    *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "LU"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita                  *
      *                                                                *
      *                              - "00" : Nessun controllo         *
      *                              - "01" : Controllo solo su licen- *
      *                                       ziatario                 *
      *                              - "02" : Controllo sia sul licen- *
      *                                       ziatario che sul numero  *
      *                                       di utenti                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * BN          Richiesta di separazione di un pathname nelle sue  *
      *             due componenti : basename e filename               *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "BN"                              *
      *                                                                *
      *                      o-pat = pathname da suddividere           *
      *                                                                *
      *                                                                *
      *             Output : o-pat = basename, comprendente il carat-  *
      *                              tere di separazione con l'ultimo  *
      *                              elemento, cioe' con il filename   *
      *                                                                *
      *                      o-com = filename                          *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * PN          Richiesta di composizione di un pathname dalle sue *
      *             due componenti : basename e filename               *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "PN"                              *
      *                                                                *
      *                      o-pat = basename, comprendente il carat-  *
      *                              tere di separazione con l'ultimo  *
      *                              elemento, cioe' con il filename   *
      *                                                                *
      *                      o-com = filename                          *
      *                                                                *
      *                                                                *
      *             Output : o-pat = pathname risultante               *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * CP          Richiesta funzione Copy File by Pathname           *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "CP"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita                  *
      *                                                                *
      *                              - Spaces : Comando eseguito       *
      *                              - ##     : Errore in esecuzione   *
      *                                                                *
      *                                                                *
      *             Note   : Presume il preventivo richiamo di A0, di  *
      *                      A5 per il pathname del file originale, e  *
      *                      di A5 per il pathname del file duplicato  *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * DP          Richiesta funzione Delete File by Pathname         *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "DP"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita                  *
      *                                                                *
      *                              - Spaces : Comando eseguito       *
      *                              - ##     : Errore in esecuzione   *
      *                                                                *
      *                                                                *
      *             Note   : Presume il preventivo richiamo di A0, e   *
      *                      di A5 per il pathname del file da cancel- *
      *                      lare                                      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * RP          Richiesta funzione Rename File by Pathname         *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "RP"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Status di uscita                  *
      *                                                                *
      *                              - Spaces : Comando eseguito       *
      *                              - ##     : Errore in esecuzione   *
      *                                                                *
      *                                                                *
      *             Note   : Presume il preventivo richiamo di A0, di  *
      *                      A5 per il pathname del file originale, e  *
      *                      di A5 per il pathname del file ridenomi-  *
      *                      nato                                      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * EX          Richiesta delle massime dimensioni in caratteri    *
      *             per un filename e per la sua estensione, ed inol-  *
      *             tre del carattere da usarsi come separazione per   *
      *             l'estensione                                       *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "EX"                              *
      *                                                                *
      *                                                                *
      *             Output : o-mxf = Massima dimensione filename       *
      *                                                                *
      *                      o-mxe = Massima dimensione estensione;    *
      *                              se estensione non ammessa sa-     *
      *                              ra' posto a zero                  *
      *                                                                *
      *                      o-che = Carattere che deve essere uti-    *
      *                              lizzato per l'estensione; se      *
      *                              estensione non ammessa sara'      *
      *                              posto a spaces                    *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * TS          Richiesta dei tipi di chiamata al sistema che pos- *
      *             sono essere eseguiti.                              *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "TS"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Tipi di chiamata che possono es-  *
      *                              sere eseguiti                     *
      *                                                                *
      *                              - Spaces : Tutti i tipi           *
      *                              - NN     : Tutti i tipi tranne la *
      *                                         richiesta di una shell *
      *                                         interattiva a comandi  *
      *                                         manuali                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * <>          Test se richiesta interruzione stampa da operatore *
      *             mediante pressione in stretta sequenza dei tasti   *
      *             funzione Pf1, Pf2, Pf3, Pf4                        *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "<>"                              *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Esito del test                    *
      *                                                                *
      *                              - Spaces : No, tutto normale      *
      *                              - <>     : Si, e' stata richiesta *
      *                                         l'interruzione della   *
      *                                         stampa mediante pres-  *
      *                                         sione degli appositi   *
      *                                         tasti                  *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * +K          Test se pressione asincrona multipla di tasti fun- *
      *             zione da operatore                                 *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "+K"                              *
      *                                                                *
      *                      o-mxf = Linea per accettazione asincrona, *
      *                              oppure zero per linea standard    *
      *                                                                *
      *                      o-mxe = Posizione per accettazione asin-  *
      *                              crona, oppure zero per posizione  *
      *                              standard                          *
      *                                                                *
      *                      o-com = Max 10 tasti funzione (4 caratte- *
      *                              ri per ogni tasto funzione, dove  *
      *                              il primo a spaces termina la se-  *
      *                              rie), che devono essere stati     *
      *                              premuti dall'operatore in stret-  *
      *                              ta sequenza.                      *
      *                                                                *
      *                                                                *
      *             Output : o-sts = Esito del test                    *
      *                                                                *
      *                              - Spaces : No, tutto normale      *
      *                              - <>     : Si, e' stata richiesta *
      *                                         l'interruzione della   *
      *                                         stampa mediante pres-  *
      *                                         sione degli appositi   *
      *                                         tasti                  *
      *                                                                *
      *             Nota   : RTRN  = Return                            *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * =K          Test se pressione asincrona singola di un tasto    *
      *             funzione da operatore                              *
      *                                                                *
      *                                                                *
      *             Input  : o-ope = "=K"                              *
      *                                                                *
      *                      o-com = Max 10 tasti funzione (4 caratte- *
      *                              ri per ogni tasto funzione, dove  *
      *                              il primo a spaces termina la se-  *
      *                              rie), uno dei quali deve essere   *
      *                              stato premuto dall'operatore      *
      *                                                                *
      *                                                                *
      *             Output : o-com = Esito del test                    *
      *                                                                *
      *                              - Spaces : No, nessuno dei tasti  *
      *                                         funzione elencati e'   *
      *                                         stato premuto          *
      *                              - XXXX   : Si, e' stata premuto   *
      *                                         il tasto funzione XXXX *
      *                                                                *
      *             Nota   : RTRN  = Return                            *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * CG          Aggiornamento system clock gap                     *
      *                                                                *
      *             Input  : o-ope = "CG"                              *
      *                                                                *
      *                      o-cdt = correttivo giorni                 *
      *                                                                *
      *                      o-chh = correttivo ore                    *
      *                                                                *
      *                                                                *
      *             Output : o-det = nessuno                           *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * DT          Estrazione system date and time attuale            *
      *                                                                *
      *             Input  : o-ope = "DT"                              *
      *                                                                *
      *             Output : o-det = System date and time attuale      *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * NS          Normalizzazione secolo nella data                  *
      *                                                                *
      *             Input  : o-ope = "NS"                              *
      *                                                                *
      *                      o-dat = Data s.aa.mm.gg da normalizzare   *
      *                                                                *
      *             Output : o-dat = Data s.aa.mm.gg con secolo        *
      *                              normalizzato                      *
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

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di interfaccia per                         "msystd"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/l"                                  .

      *    *===========================================================*
      *    * Informazioni mantenute internamente dal programma         *
      *    *-----------------------------------------------------------*
       01  z.
      *        *-------------------------------------------------------*
      *        * Pathname per il richiamo del modulo specifico         *
      *        *-------------------------------------------------------*
           05  z-mod-ops.
      *            *---------------------------------------------------*
      *            * Pathname per il richiamo del modulo specifico     *
      *            *---------------------------------------------------*
               10  z-pat-ops              pic  x(20) value
                       "swd/mod/prg/obj/mops"                         .
      *            *---------------------------------------------------*
      *            * Codice del sistema operativo ospite               *
      *            *---------------------------------------------------*
               10  z-cod-ops              pic  9(02) value zero       .

      *    *===========================================================*
      *    * System clock gap                                          *
      *    *-----------------------------------------------------------*
       01  w-scg.
      *        *-------------------------------------------------------*
      *        * Correttivi                                            *
      *        *-------------------------------------------------------*
           05  w-scg-cdt                  pic s9(02) value 00         .
           05  w-scg-chh                  pic s9(02) value 00         .

      *    *===========================================================*
      *    * System information                                        *
      *    *-----------------------------------------------------------*
       01  w-sys-inf.
      *        *-------------------------------------------------------*
      *        * Operating system                                      *
      *        * -    Unix-V                                           *
      *        * -    Unix-4                                           *
      *        * -    UNOS                                             *
      *        * -    VAX/VMS                                          *
      *        * -    MS-DOS                                           *
      *        * -    WINDOWS                                          *
      *        * -    OS/2                                             *
      *        * -    AOS/VS                                           *
      *        *-------------------------------------------------------*
           05  w-sys-inf-ope-sys          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * User-Id                                               *
      *        *-------------------------------------------------------*
           05  w-sys-inf-usr-ide          pic  x(12)                  .
      *        *-------------------------------------------------------*
      *        * Station-Id                                            *
      *        *-------------------------------------------------------*
           05  w-sys-inf-sta-ide          pic  x(12)                  .
      *        *-------------------------------------------------------*
      *        * Has indexed read previous                             *
      *        *-------------------------------------------------------*
           05  w-sys-inf-has-irp          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Has relative read previous                            *
      *        *-------------------------------------------------------*
           05  w-sys-inf-has-rrp          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Can test input status                                 *
      *        *-------------------------------------------------------*
           05  w-sys-inf-can-tis          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Is multi-tasking                                      *
      *        *-------------------------------------------------------*
           05  w-sys-inf-mul-tsk          pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      ******************************************************************
       Procedure Division                using o                      .
      ******************************************************************

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Esecuzione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        o-ope                =    "OP"
                     perform opn-ops-000  thru opn-ops-999
           else if   o-ope                =    "CL"
                     perform cls-ops-000  thru cls-ops-999
           else if   o-ope                =    "CG"
                     perform clk-gap-000  thru clk-gap-999
           else if   o-ope                =    "DT"
                     perform est-det-000  thru est-det-999
           else if   o-ope                =    "NS"
                     perform nor-sec-000  thru nor-sec-999
           else      perform mop-sxx-000  thru mop-sxx-999            .
       main-999.
           exit      program.

      *================================================================*
      *       Routines                                                 *
      *----------------------------------------------------------------*

      *    *===========================================================*
      *    * Open modulo                                               *
      *    *                                                           *
      *    * Note : Questa funzione viene richiamata dal programma     *
      *    *        pxpg0001, inizializzazione esecuzione in fore-     *
      *    *        ground, per il test che il sistema operativo       *
      *    *        sia di tipo riconosciuto                           *
      *    *                                                           *
      *    *        Questa funzione viene richiamata dal programma     *
      *    *        pxpg0800, inizializzazione esecuzione in back-     *
      *    *        ground, senza il test che il sistema operativo     *
      *    *        sia di tipo riconosciuto                           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       opn-ops-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
      *              *-------------------------------------------------*
      *              * Raccolta delle system-informations              *
      *              *-------------------------------------------------*
           accept    w-sys-inf            from system-info            .
      *              *-------------------------------------------------*
      *              * Preparazione della sigla del sistema operativo  *
      *              * in uscita                                       *
      *              *-------------------------------------------------*
           move      w-sys-inf-ope-sys
                                          to   o-oss                  .
      *              *-------------------------------------------------*
      *              * Preparazione degli altri valori in uscita       *
      *              *                                                 *
      *              * - Codice interno del sistema operativo          *
      *              * - Descrizione del sistema operativo             *
      *              *-------------------------------------------------*
           if        o-oss                =    "Unix-V    "
                     move  01             to   o-osc
                     move  "Unix System V                 "
                                          to   o-osd
           else if   o-oss                =    "Unix-4    " or
                     o-oss                =    "Unix      "
                     move  02             to   o-osc
                     move  "Berkeley Unix                 "
                                          to   o-osd
           else if   o-oss                =    "VAX-VMS   "
                     move  03             to   o-osc
                     move  "Digital Equipment VMS         "
                                          to   o-osd
           else if   o-oss                =    "MS-DOS    " or
                     o-oss                =    "WINDOWS   "
                     move  04             to   o-osc
                     move  "Microsoft MS-DOS              "
                                          to   o-osd
           else      move  zero           to   o-osc
                     move  spaces         to   o-osd                  .
      *              *-------------------------------------------------*
      *              * Bufferizzazione del codice interno del sistema  *
      *              * operativo ospite                                *
      *              *-------------------------------------------------*
           move      o-osc                to   z-cod-ops              .
      *              *-------------------------------------------------*
      *              * Se sistema operativo non riconosciuto si esce   *
      *              * con status di uscita a : SX                     *
      *              *-------------------------------------------------*
           if        z-cod-ops            =    zero
                     move  "SX"           to   o-sts
                     go to opn-ops-999.
      *              *-------------------------------------------------*
      *              * Richiamo della funzione 'OP' per il sistema o-  *
      *              * perativo ospite                                 *
      *              *-------------------------------------------------*
           perform   mop-sxx-000          thru mop-sxx-999            .
       opn-ops-999.
           exit.

      *    *===========================================================*
      *    * Close modulo                                              *
      *    *                                                           *
      *    * Note : Questa funzione viene richiamata dal programma     *
      *    *        pxpg0009, terminazione esecuzione in foreground    *
      *    *                                                           *
      *    *        Questa funzione viene richiamata dal programma     *
      *    *        pxpg0800, inizializzazione esecuzione in back-     *
      *    *        ground, senza il test che il sistema operativo     *
      *    *        sia di tipo riconosciuto                           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cls-ops-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   o-sts                  .
      *              *-------------------------------------------------*
      *              * Se sistema operativo non riconosciuto si esce   *
      *              * senza alcuna azione                             *
      *              *-------------------------------------------------*
           if        z-cod-ops            =    zero
                     go to cls-ops-999.
      *              *-------------------------------------------------*
      *              * Richiamo della funzione 'CL' per il sistema o-  *
      *              * perativo ospite                                 *
      *              *-------------------------------------------------*
           perform   mop-sxx-000          thru mop-sxx-999            .
       cls-ops-999.
           exit.

      *    *===========================================================*
      *    * System clock gap                                          *
      *    *                                                           *
      *    * Note : Questa funzione viene richiamata dal programma     *
      *    *        pxpg0001, per preparare i correttivi eventuali     *
      *    *        al clock di sistema                                *
      *    *-----------------------------------------------------------*
       clk-gap-000.
           move      o-cdt                to   w-scg-cdt              .
           move      o-chh                to   w-scg-chh              .
       clk-gap-999.
           exit.

      *    *===========================================================*
      *    * Estrazione system date and time                           *
      *    *                                                           *
      *    * Note : Questa funzione e la successiva, erano nei moduli  *
      *    *        specifici ma sono state riportate nel modulo ge-   *
      *    *        nerico                                             *
      *    *-----------------------------------------------------------*
       est-det-000.
      *              *-------------------------------------------------*
      *              * Accettazione system date and time               *
      *              *-------------------------------------------------*
           move      "DT"                 to   l-ope                  .
           move      w-scg-cdt            to   l-cdt
           move      w-scg-chh            to   l-chh
           call      "swd/mod/prg/obj/msystd"
                                         using l                      .
           move      l-det                to   o-det                  .
       est-det-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione secolo nella data                         *
      *    *-----------------------------------------------------------*
       nor-sec-000.
      *              *-------------------------------------------------*
      *              * Accettazione system date and time               *
      *              *-------------------------------------------------*
           move      "NS"                 to   l-ope                  .
           move      o-dat                to   l-dat                  .
           call      "swd/mod/prg/obj/msystd"
                                         using l                      .
           move      l-det                to   o-det                  .
       nor-sec-999.
           exit.

      *    *===========================================================*
      *    * Altre funzioni diverse da 'OP' e 'CL'                     *
      *    *-----------------------------------------------------------*
       mop-sxx-000.
           call      z-mod-ops           using o
                                               w-sys-inf              .
       mop-sxx-999.
           exit.
