       Identification Division.
       Program-Id.                                 mtsubf             .
      *================================================================*
      *                                                                *
      *      Modulo per l'impaccamento o disimpaccamento di files      *
      *                                                                *
      *      N.B.: Usato in passato per trasferimenti dati tra Sede e  *
      *            Dipendenze (Utilizzato in area 'gsr' di 'trc')      *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Open output file packed                            *
      * Output                                                         *
      *             Input  : t-ope     : "OO"                          *
      *                                                                *
      *                      t-fil-nam : Nome simbolico del file pac-  *
      *                                  ked che deve essere generato  *
      *                                                                *
      *                      t-fil-pat : Pathname del file packed che  *
      *                                  deve essere generato          *
      *                                                                *
      *                      t-org-sna : System-name della macchina di *
      *                                  origine, se a spaces signifi- *
      *                                  ca che non e' significativo   *
      *                                                                *
      *                      t-org-dat : System-date nella macchina di *
      *                                  origine al momento dell'ini-  *
      *                                  zio operazione                *
      *                                                                *
      *                      t-org-tim : System-time nella macchina di *
      *                                  origine al momento dell'ini-  *
      *                                  zio operazione                *
      *                                                                *
      *                      t-org-azi : Codice azienda relativo ai    *
      *                                  files che devono essere im-   *
      *                                  paccati nella macchina di o-  *
      *                                  rigine, se a spaces signifi-  *
      *                                  ca che non e' significativo   *
      *                                                                *
      *                      t-org-ute : Codice utente che ha operato  *
      *                                  l'impaccamento, se a spaces   *
      *                                  significa che non e' signifi- *
      *                                  cativo                        *
      *                                                                *
      *                      t-org-ter : Codice terminale da cui e' i- *
      *                                  niziato l'operazione di im-   *
      *                                  paccamento, se a spaces si-   *
      *                                  gnifica che non e' signifi-   *
      *                                  cativo                        *
      *                                                                *
      *                      t-org-sap : Codice sistema applicativo    *
      *                                  cui appartiene il programma   *
      *                                  che richiede l'impaccamento,  *
      *                                  se a spaces significa che     *
      *                                  non e' significativo          *
      *                                                                *
      *                      t-org-arg : Codice area gestionale        *
      *                                  cui appartiene il programma   *
      *                                  che richiede l'impaccamento,  *
      *                                  se a spaces significa che     *
      *                                  non e' significativo          *
      *                                                                *
      *                      t-org-set : Codice settore gestionale     *
      *                                  cui appartiene il programma   *
      *                                  che richiede l'impaccamento,  *
      *                                  se a spaces significa che     *
      *                                  non e' significativo          *
      *                                                                *
      *                      t-org-fas : Codice fase gestionale        *
      *                                  cui appartiene il programma   *
      *                                  che richiede l'impaccamento,  *
      *                                  se a spaces significa che     *
      *                                  non e' significativo          *
      *                                                                *
      *                      t-org-pro : Sigla interna del programma   *
      *                                  che richiede l'impaccamento,  *
      *                                  se a spaces significa che     *
      *                                  non e' significativo          *
      *                                                                *
      *                      t-dst-sna : System-name della macchina di *
      *                                  destinazione, se a spaces si- *
      *                                  gnifica che non e' significa- *
      *                                  tivo                          *
      *                                                                *
      *                      t-dst-azi : Codice azienda relativo ai    *
      *                                  files che devono essere di-   *
      *                                  simpaccati nella macchina di  *
      *                                  destinazione, se a spaces si- *
      *                                  gnifica che non e' significa- *
      *                                  tivo, e che i files possono   *
      *                                  essere disimpaccati in qual-  *
      *                                  siasi azienda                 *
      *                                                                *
      *                      t-dst-ute : Codice utente nella macchina  *
      *                                  di destinazione che ha dirit- *
      *                                  to ad effettuare il disimpac- *
      *                                  camento, se a spaces signifi- *
      *                                  gnifica che qualsiasi utente  *
      *                                  puo' operare il disimpacca-   *
      *                                  mento                         *
      *                                                                *
      *                      t-dst-ter : Codice terminale nella mac-   *
      *                                  china di destinazione che ha  *
      *                                  diritto ad effettuare il di-  *
      *                                  simpaccamento, se a spaces    *
      *                                  significa che il disimpacca-  *
      *                                  mento puo' essere operato da  *
      *                                  qualsiasi terminale           *
      *                                                                *
      *                      t-dst-psw : Password da richiedere a chi  *
      *                                  effettua il disimpaccamento,  *
      *                                  se a spaces non sara' richie- *
      *                                  sta alcuna password           *
      *                                                                *
      *                      t-dst-iad : Informazioni addizionali per  *
      *                                  il programma che effettuera'  *
      *                                  il disimpaccamento, se a spa- *
      *                                  ces significa che non ci sono *
      *                                  informazioni addizionali      *
      *                                                                *
      *                      t-dst-msg : Messaggio da evidenziare al-  *
      *                                  l'operatore che effettuera'   *
      *                                  il disimpaccamento, se a spa- *
      *                                  ces non sara' evidenziato al- *
      *                                  cun messaggio                 *
      *                                    t-tsd-m01 : linea 1         *
      *                                    t-tsd-m02 : linea 2         *
      *                                    t-tsd-m03 : linea 3         *
      *                                    t-tsd-m04 : linea 4         *
      *                                    t-tsd-m05 : linea 5         *
      *                                                                *
      *             Output : t-sts     : Status di uscita              *
      *                                                                *
      *                                  e-not-err : nessun errore     *
      *                                  e-use-err : file locked       *
      *                                  e-fil-inc : il file non ha il *
      *                                              formato corretto  *
      *                                  e-opn-err : pathname errato   *
      *                                  e-fat-err : fatal error       *
      *                                                                *
      *                      t-err     : Messaggio di errore           *
      *                                                                *
      *                                  t-e01     : linea 1           *
      *                                  t-e02     : linea 2           *
      *                                  t-e03     : linea 3           *
      *                                  t-e04     : linea 4           *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Open input file packed                             *
      * Input                                                          *
      *             Input  : t-ope     : "OI"                          *
      *                                                                *
      *                      t-fil-nam : Nome simbolico del file pac-  *
      *                                  ked che deve essere letto     *
      *                                                                *
      *                      t-fil-pat : Pathname del file packed che  *
      *                                  deve essere letto             *
      *                                                                *
      *             Output : t-sts     : Status di uscita              *
      *                                                                *
      *                                  e-not-err : nessun errore     *
      *                                  e-use-err : file locked       *
      *                                  e-fil-inc : il file non ha il *
      *                                              formato corretto  *
      *                                  e-opn-err : pathname errato   *
      *                                  e-fat-err : fatal error       *
      *                                                                *
      *                      t-err     : Messaggio di errore           *
      *                                                                *
      *                                  t-e01     : linea 1           *
      *                                  t-e02     : linea 2           *
      *                                  t-e03     : linea 3           *
      *                                  t-e04     : linea 4           *
      *                                                                *
      *                      t-fil-nam : Nome simbolico del file pac-  *
      *                                  ked al momento della sua ge-  *
      *                                  nerazione                     *
      *                                                                *
      *                      t-fil-pat : Pathname del file packed al   *
      *                                  momento della sua generazione *
      *                                                                *
      *                      t-org-sna : System-name della macchina di *
      *                                  origine, se a spaces signifi- *
      *                                  ca che non e' significativo   *
      *                                                                *
      *                      t-org-dat : System-date nella macchina di *
      *                                  origine al momento dell'ini-  *
      *                                  zio operazione                *
      *                                                                *
      *                      t-org-tim : System-time nella macchina di *
      *                                  origine al momento dell'ini-  *
      *                                  zio operazione                *
      *                                                                *
      *                      t-org-azi : Codice azienda relativo ai    *
      *                                  files che sono stati impacca- *
      *                                  ti nella macchina di origine, *
      *                                  se a spaces significa che non *
      *                                  e' significativo              *
      *                                                                *
      *                      t-org-ute : Codice utente che ha operato  *
      *                                  l'impaccamento sulla macchina *
      *                                  di origine, se a spaces si-   *
      *                                  gnifica che non e' significa- *
      *                                  tivo                          *
      *                                                                *
      *                      t-org-ter : Codice terminale da cui e' i- *
      *                                  niziato l'operazione di im-   *
      *                                  paccamento nella macchina di  *
      *                                  origine, se a spaces signifi- *
      *                                  ca che non e' significativo   *
      *                                                                *
      *                      t-org-sap : Codice sistema applicativo    *
      *                                  cui appartiene il programma   *
      *                                  che ha richiesto l'impacca-   *
      *                                  mento nella macchina di ori-  *
      *                                  gine, se a spaces significa   *
      *                                  che non e' significativo      *
      *                                                                *
      *                      t-org-arg : Codice area gestionale        *
      *                                  cui appartiene il programma   *
      *                                  che ha richiesto l'impacca-   *
      *                                  mento nella macchina di ori-  *
      *                                  gine, se a spaces significa   *
      *                                  che non e' significativo      *
      *                                                                *
      *                      t-org-set : Codice settore gestionale     *
      *                                  cui appartiene il programma   *
      *                                  che ha richiesto l'impacca-   *
      *                                  mento nella macchina di ori-  *
      *                                  gine, se a spaces significa   *
      *                                  che non e' significativo      *
      *                                                                *
      *                      t-org-fas : Codice fase gestionale        *
      *                                  cui appartiene il programma   *
      *                                  che ha richiesto l'impacca-   *
      *                                  mento nella macchina di ori-  *
      *                                  gine, se a spaces significa   *
      *                                  che non e' significativo      *
      *                                                                *
      *                      t-org-pro : Sigla interna del programma   *
      *                                  che ha richiesto l'impacca-   *
      *                                  mento nella macchina di ori-  *
      *                                  gine, se a spaces significa
      *                                  che non e' significativo      *
      *                                                                *
      *                      t-dst-sna : System-name della macchina di *
      *                                  destinazione, se a spaces si- *
      *                                  gnifica che non e' significa- *
      *                                  tivo                          *
      *                                                                *
      *                      t-dst-azi : Codice azienda relativo ai    *
      *                                  files che devono essere di-   *
      *                                  simpaccati nella macchina di  *
      *                                  destinazione, se a spaces si- *
      *                                  gnifica che non e' significa- *
      *                                  tivo, e che i files possono   *
      *                                  essere disimpaccati in qual-  *
      *                                  siasi azienda                 *
      *                                                                *
      *                      t-dst-ute : Codice utente nella macchina  *
      *                                  di destinazione che ha dirit- *
      *                                  to ad effettuare il disimpac- *
      *                                  camento, se a spaces signifi- *
      *                                  gnifica che qualsiasi utente  *
      *                                  puo' operare il disimpacca-   *
      *                                  mento                         *
      *                                                                *
      *                      t-dst-ter : Codice terminale nella mac-   *
      *                                  china di destinazione che ha  *
      *                                  diritto ad effettuare il di-  *
      *                                  simpaccamento, se a spaces    *
      *                                  significa che il disimpacca-  *
      *                                  mento puo' essere operato da  *
      *                                  qualsiasi terminale           *
      *                                                                *
      *                      t-dst-psw : Password da richiedere a chi  *
      *                                  effettua il disimpaccamento,  *
      *                                  se a spaces non sara' richie- *
      *                                  sta alcuna password           *
      *                                                                *
      *                      t-dst-iad : Informazioni addizionali per  *
      *                                  il programma che effettuera'  *
      *                                  il disimpaccamento, se a spa- *
      *                                  ces significa che non ci sono *
      *                                  informazioni addizionali      *
      *                                                                *
      *                      t-dst-msg : Messaggio da evidenziare al-  *
      *                                  l'operatore che effettuera'   *
      *                                  il disimpaccamento, se a spa- *
      *                                  ces non sara' evidenziato al- *
      *                                  cun messaggio                 *
      *                                    t-tsd-m01 : linea 1         *
      *                                    t-tsd-m02 : linea 2         *
      *                                    t-tsd-m03 : linea 3         *
      *                                    t-tsd-m04 : linea 4         *
      *                                    t-tsd-m05 : linea 5         *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Close file packed                                  *
      *                                                                *
      *             Input  : t-ope     : "CL"                          *
      *                                                                *
      *             Output : t-sts     : Status di uscita              *
      *                                                                *
      *                                  e-not-err : nessun errore     *
      *                                  e-fat-err : fatal error       *
      *                                                                *
      *                      t-err     : Messaggio di errore           *
      *                                                                *
      *                                  t-e01     : linea 1           *
      *                                  t-e02     : linea 2           *
      *                                  t-e03     : linea 3           *
      *                                  t-e04     : linea 4           *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put         Scrittura record in file packed                    *
      * Next                                                           *
      *             Input  : t-ope     : "PN"                          *
      *                                                                *
      *                      t-sub-sgl : Sigla del subfile del record  *
      *                                  che deve essere scritto       *
      *                                                                *
      *                      t-sub-rec : Contenuto del record che deve *
      *                                  essere scritto                *
      *                                                                *
      *             Output : t-sts     : Status di uscita              *
      *                                                                *
      *                                  e-not-err : nessun errore     *
      *                                  e-fat-err : fatal error       *
      *                                                                *
      *                      t-err     : Messaggio di errore           *
      *                                                                *
      *                                  t-e01     : linea 1           *
      *                                  t-e02     : linea 2           *
      *                                  t-e03     : linea 3           *
      *                                  t-e04     : linea 4           *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Get         Lettura record da file packed                      *
      * Next                                                           *
      *             Input  : t-ope     : "GN"                          *
      *                                                                *
      *             Output : t-sts     : Status di uscita              *
      *                                                                *
      *                                  e-not-err : nessun errore     *
      *                                  e-end-fil : at end            *
      *                                  e-fat-err : fatal error       *
      *                                                                *
      *                      t-err     : Messaggio di errore           *
      *                                                                *
      *                                  t-e01     : linea 1           *
      *                                  t-e02     : linea 2           *
      *                                  t-e03     : linea 3           *
      *                                  t-e04     : linea 4           *
      *                                                                *
      *                      t-sub-sgl : Sigla del subfile del record  *
      *                                  che e' stato letto            *
      *                                                                *
      *                      t-sub-rec : Contenuto del record che e'   *
      *                                  stato letto                   *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Area per identificazione versione del programma           *
      *    *-----------------------------------------------------------*
       01  w-ide.
      *        *-------------------------------------------------------*
      *        * Label iniziale file packed                            *
      *        *-------------------------------------------------------*
           05  w-ide-lbl                  pic  x(10)
                     value "altertsubf"                               .
      *        *-------------------------------------------------------*
      *        * Versione file packed                                  *
      *        *-------------------------------------------------------*
           05  w-ide-vrs                  pic  x(04)
                     value "0000"                                     .

      *    *===========================================================*
      *    * Work per valori dell'area di preambolo del file           *
      *    *-----------------------------------------------------------*
       01  w-wat.
      *        *-------------------------------------------------------*
      *        * File                                                  *
      *        *-------------------------------------------------------*
           05  w-wat-fil.
      *            *---------------------------------------------------*
      *            * Nome simbolico del file packed                    *
      *            *---------------------------------------------------*
               10  w-wat-fil-nam          pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Pathname del file packed                          *
      *            *---------------------------------------------------*
               10  w-wat-fil-pat          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Origine                                               *
      *        *-------------------------------------------------------*
           05  w-wat-org.
      *            *---------------------------------------------------*
      *            * Nome del sistema                                  *
      *            *---------------------------------------------------*
               10  w-wat-org-sna          pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Data s.aa.mm.gg                                   *
      *            *---------------------------------------------------*
               10  w-wat-org-dat          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ora hh.mm.ss.cc                                   *
      *            *---------------------------------------------------*
               10  w-wat-org-tim          pic  9(08)                  .
      *            *---------------------------------------------------*
      *            * Codice azienda                                    *
      *            *---------------------------------------------------*
               10  w-wat-org-azi          pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Codice utente                                     *
      *            *---------------------------------------------------*
               10  w-wat-org-ute          pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Codice terminale                                  *
      *            *---------------------------------------------------*
               10  w-wat-org-ter          pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Sistema applicativo                               *
      *            *---------------------------------------------------*
               10  w-wat-org-sap          pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Area gestionale                                   *
      *            *---------------------------------------------------*
               10  w-wat-org-arg          pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Settore gestionale                                *
      *            *---------------------------------------------------*
               10  w-wat-org-set          pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Fase gestionale                                   *
      *            *---------------------------------------------------*
               10  w-wat-org-fas          pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Sigla interna del programma                       *
      *            *---------------------------------------------------*
               10  w-wat-org-pro          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Destinazione                                          *
      *        *-------------------------------------------------------*
           05  w-wat-dst.
      *            *---------------------------------------------------*
      *            * Nome del sistema                                  *
      *            *---------------------------------------------------*
               10  w-wat-dst-sna          pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Codice azienda                                    *
      *            *---------------------------------------------------*
               10  w-wat-dst-azi          pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Codice utente                                     *
      *            *---------------------------------------------------*
               10  w-wat-dst-ute          pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Codice terminale                                  *
      *            *---------------------------------------------------*
               10  w-wat-dst-ter          pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Password per disimpaccamento                      *
      *            *---------------------------------------------------*
               10  w-wat-dst-psw          pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Informazioni addizionali                          *
      *            *---------------------------------------------------*
               10  w-wat-dst-iad          pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Messaggio da mostrare a chi effettua il disim-    *
      *            * paccamento                                        *
      *            *---------------------------------------------------*
               10  w-wat-dst-msg.
                   15  w-wat-dst-m01      pic  x(76)                  .
                   15  w-wat-dst-m02      pic  x(76)                  .
                   15  w-wat-dst-m03      pic  x(76)                  .
                   15  w-wat-dst-m04      pic  x(76)                  .
                   15  w-wat-dst-m05      pic  x(76)                  .

      *    *===========================================================*
      *    * Area per ridefinizione label e vesrsione letti            *
      *    *-----------------------------------------------------------*
       01  w-wid.
           05  w-wid-lbl                  pic  x(10)                  .
           05  w-wid-vrs                  pic  x(04)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mtsubf"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/t"                                  .

      ******************************************************************
       Procedure Division                using t                      .
      ******************************************************************

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita e messaggio di *
      *              * errore in uscita                                *
      *              *-------------------------------------------------*
           move      e-not-err            to   t-sts                  .
           move      spaces               to   t-err                  .
      *              *-------------------------------------------------*
      *              * Test su tipo operazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open output                                 *
      *                  *---------------------------------------------*
           if        t-ope                =    "OO"
                     perform opn-out-000  thru opn-out-999
      *                  *---------------------------------------------*
      *                  * Open input                                  *
      *                  *---------------------------------------------*
           else if   t-ope                =    "OI"
                     perform opn-inp-000  thru opn-inp-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   t-ope                =    "CL"
                     perform cls-fil-000  thru cls-fil-999
      *                  *---------------------------------------------*
      *                  * Put next                                    *
      *                  *---------------------------------------------*
           else if   t-ope                =    "PN"
                     perform put-nxt-000  thru put-nxt-999
      *                  *---------------------------------------------*
      *                  * Get next                                    *
      *                  *---------------------------------------------*
           else if   t-ope                =    "GN"
                     perform get-nxt-000  thru get-nxt-999            .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Open output                                               *
      *    *-----------------------------------------------------------*
       opn-out-000.
      *              *-------------------------------------------------*
      *              * Open [pkf] in output                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione del name                       *
      *                  *---------------------------------------------*
           move      "pkf "               to   g-nam                  .
      *                  *---------------------------------------------*
      *                  * Preparazione del pathname                   *
      *                  *---------------------------------------------*
           move      t-fil-pat            to   g-pat                  .
      *                  *---------------------------------------------*
      *                  * Open output                                 *
      *                  *---------------------------------------------*
           move      "OO"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to opn-out-200.
      *              *-------------------------------------------------*
      *              * Se errori di i-o                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nome file errato, o se file locked, o se *
      *                  * file esistente ma di formato inaspettato si *
      *                  * ritorna lo status di errore; ogni altro ti- *
      *                  * po di errore e' considerato un fatal error, *
      *                  * e si ritorna lo status di fatal error       *
      *                  *---------------------------------------------*
           if        g-sts                =    e-opn-err or
                     g-sts                =    e-use-err or
                     g-sts                =    e-fil-inc
                     go to opn-out-050
           else      go to opn-out-100.
       opn-out-050.
      *                  *---------------------------------------------*
      *                  * Errore riconosciuto                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di i-o error                  *
      *                      *-----------------------------------------*
           perform   msg-ioe-err-000      thru msg-ioe-err-999        .
      *                      *-----------------------------------------*
      *                      * Status in uscita                        *
      *                      *-----------------------------------------*
           move      g-sts                to   t-sts                  .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [pkf]                  *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione modulo                    *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     opn-out-999.
       opn-out-100.
      *                  *---------------------------------------------*
      *                  * Errore non riconosciuto                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di i-o error                  *
      *                      *-----------------------------------------*
           perform   msg-ioe-err-000      thru msg-ioe-err-999        .
      *                      *-----------------------------------------*
      *                      * Status di fatal error in uscita         *
      *                      *-----------------------------------------*
           move      e-fat-err            to   t-sts                  .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [pkf]                  *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione modulo                    *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     opn-out-999.
       opn-out-200.
      *              *-------------------------------------------------*
      *              * Se operazione di i-o correttamente eseguita     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Output label e versione                     *
      *                  *---------------------------------------------*
           perform   out-lbl-vrs-000      thru out-lbl-vrs-999        .
           if        t-sts                not  = e-not-err
                     go to opn-out-999.
      *                  *---------------------------------------------*
      *                  * Output area w-wat                           *
      *                  *---------------------------------------------*
           perform   out-are-wat-000      thru out-are-wat-999        .
           if        t-sts                not  = e-not-err
                     go to opn-out-999.
       opn-out-999.
           exit.

      *    *===========================================================*
      *    * Open input                                                *
      *    *-----------------------------------------------------------*
       opn-inp-000.
      *              *-------------------------------------------------*
      *              * Open [pkf] in input                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione del name                       *
      *                  *---------------------------------------------*
           move      "pkf "               to   g-nam                  .
      *                  *---------------------------------------------*
      *                  * Preparazione del pathname                   *
      *                  *---------------------------------------------*
           move      t-fil-pat            to   g-pat                  .
      *                  *---------------------------------------------*
      *                  * Open output                                 *
      *                  *---------------------------------------------*
           move      "OI"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to opn-inp-200.
      *              *-------------------------------------------------*
      *              * Se errori di i-o                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nome file errato, o se file locked, o se *
      *                  * file esistente ma di formato inaspettato si *
      *                  * ritorna lo status di errore; ogni altro ti- *
      *                  * po di errore e' considerato un fatal error, *
      *                  * e si ritorna lo status di fatal error       *
      *                  *---------------------------------------------*
           if        g-sts                =    e-opn-err or
                     g-sts                =    e-use-err or
                     g-sts                =    e-fil-inc
                     go to opn-inp-050
           else      go to opn-inp-100.
       opn-inp-050.
      *                  *---------------------------------------------*
      *                  * Errore riconosciuto                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di i-o error                  *
      *                      *-----------------------------------------*
           perform   msg-ioe-err-000      thru msg-ioe-err-999        .
      *                      *-----------------------------------------*
      *                      * Status in uscita                        *
      *                      *-----------------------------------------*
           move      g-sts                to   t-sts                  .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [pkf]                  *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione modulo                    *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     opn-inp-999.
       opn-inp-100.
      *                  *---------------------------------------------*
      *                  * Errore non riconosciuto                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di i-o error                  *
      *                      *-----------------------------------------*
           perform   msg-ioe-err-000      thru msg-ioe-err-999        .
      *                      *-----------------------------------------*
      *                      * Status di fatal error in uscita         *
      *                      *-----------------------------------------*
           move      e-fat-err            to   t-sts                  .
      *                      *-----------------------------------------*
      *                      * Chiusura forzata [pkf]                  *
      *                      *-----------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Cancellazione modulo                    *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     opn-inp-999.
       opn-inp-200.
      *              *-------------------------------------------------*
      *              * Se operazione di i-o correttamente eseguita     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Input label e versione                      *
      *                  *---------------------------------------------*
           perform   inp-lbl-vrs-000      thru inp-lbl-vrs-999        .
           if        t-sts                not  = e-not-err
                     go to opn-inp-999.
      *                  *---------------------------------------------*
      *                  * Input area w-wat                            *
      *                  *---------------------------------------------*
           perform   inp-are-wat-000      thru inp-are-wat-999        .
           if        t-sts                not  = e-not-err
                     go to opn-inp-999.
       opn-inp-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-fil-000.
      *              *-------------------------------------------------*
      *              * Chiusura [pkf]                                  *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                            *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *              *-------------------------------------------------*
      *              * Test su esito operazione di i-o                 *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to cls-fil-999.
      *              *-------------------------------------------------*
      *              * Se errori di i-o : comunque fatal error         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di i-o error                      *
      *                  *---------------------------------------------*
           perform   msg-ioe-err-000      thru msg-ioe-err-999        .
      *                  *---------------------------------------------*
      *                  * Status di fatal error in uscita             *
      *                  *---------------------------------------------*
           move      e-fat-err            to   t-sts                  .
       cls-fil-999.
           exit.

      *    *===========================================================*
      *    * Put next                                                  *
      *    *-----------------------------------------------------------*
       put-nxt-000.
      *              *-------------------------------------------------*
      *              * Preparazione record in area file                *
      *              *-------------------------------------------------*
           move      t-sub                to   g-rec                  .
      *              *-------------------------------------------------*
      *              * Operazione di i-o                               *
      *              *-------------------------------------------------*
           perform   wrt-rec-pkf-000      thru wrt-rec-pkf-999        .
       put-nxt-999.
           exit.

      *    *===========================================================*
      *    * Get next                                                  *
      *    *-----------------------------------------------------------*
       get-nxt-000.
      *              *-------------------------------------------------*
      *              * Lettura area t-sub                              *
      *              *-------------------------------------------------*
           perform   rea-rec-pkf-000      thru rea-rec-pkf-999        .
           if        t-sts                not  = e-not-err
                     go to get-nxt-999.
      *              *-------------------------------------------------*
      *              * Decomposizione                                  *
      *              *-------------------------------------------------*
           move      g-rec                to   t-sub                  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     get-nxt-999.
       get-nxt-999.
           exit.

      *    *===========================================================*
      *    * Output label e versione                                   *
      *    *-----------------------------------------------------------*
       out-lbl-vrs-000.
      *              *-------------------------------------------------*
      *              * Composizione                                    *
      *              *-------------------------------------------------*
           move      w-ide                to   g-rec                  .
      *              *-------------------------------------------------*
      *              * Scrittura                                       *
      *              *-------------------------------------------------*
           perform   wrt-rec-pkf-000      thru wrt-rec-pkf-999        .
       out-lbl-vrs-999.
           exit.

      *    *===========================================================*
      *    * Input label e versione                                    *
      *    *-----------------------------------------------------------*
       inp-lbl-vrs-000.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           perform   rea-rec-pkf-000      thru rea-rec-pkf-999        .
           if        t-sts                not  = e-not-err
                     go to   inp-lbl-vrs-300.
      *              *-------------------------------------------------*
      *              * Controllo label e versione                      *
      *              *-------------------------------------------------*
           move      g-rec                to   w-wid                  .
           if        w-wid-lbl            =    w-ide-lbl and
                     w-wid-vrs            =    w-wid-vrs
                     go to inp-lbl-vrs-999.
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
           if        w-wid-lbl            =    w-ide-lbl
                     go to inp-lbl-vrs-200.
      *                  *---------------------------------------------*
      *                  * Se errata label                             *
      *                  *---------------------------------------------*
           move      spaces               to   t-e01                  .
           string    "Il file in input '"
                                delimited by   size
                     g-pat      delimited by   spaces
                     "' ha un formato interno inaspettato !"
                                delimited by   size
                                          into t-e01                  .
           go to     inp-lbl-vrs-300.
       inp-lbl-vrs-200.
      *                  *---------------------------------------------*
      *                  * Se errata versione                          *
      *                  *---------------------------------------------*
           move      spaces               to   t-e01                  .
           string    "Il file in input '"
                                delimited by   size
                     g-pat      delimited by   spaces
                     "' ha la versione ["
                                delimited by   size
                     w-wid-vrs  delimited by size
                     "] anziche' ["
                                delimited by   size
                     w-ide-vrs  delimited by   size
                     "] !"      delimited by   size
                                          into t-e01                  .
           go to     inp-lbl-vrs-999.
       inp-lbl-vrs-300.
      *                  *---------------------------------------------*
      *                  * Test se at end o fatal error                *
      *                  *---------------------------------------------*
           if        t-sts                not  = e-end-fil
                     go to inp-lbl-vrs-600.
      *                  *---------------------------------------------*
      *                  * Chiusura forzata [pkf]                      *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
       inp-lbl-vrs-600.
      *                  *---------------------------------------------*
      *                  * Status di uscita a fatal error              *
      *                  *---------------------------------------------*
           move      e-fat-err            to   t-sts                  .
       inp-lbl-vrs-999.
           exit.

      *    *===========================================================*
      *    * Output area w-wat                                         *
      *    *-----------------------------------------------------------*
       out-are-wat-000.
      *              *-------------------------------------------------*
      *              * Composizione                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-wat                  .
           move      t-fil-nam            to   w-wat-fil-nam          .
           move      t-fil-pat            to   w-wat-fil-pat          .
           move      t-org-sna            to   w-wat-org-sna          .
           move      t-org-dat            to   w-wat-org-dat          .
           move      t-org-tim            to   w-wat-org-tim          .
           move      t-org-azi            to   w-wat-org-azi          .
           move      t-org-ute            to   w-wat-org-ute          .
           move      t-org-ter            to   w-wat-org-ter          .
           move      t-org-sap            to   w-wat-org-sap          .
           move      t-org-arg            to   w-wat-org-arg          .
           move      t-org-set            to   w-wat-org-set          .
           move      t-org-fas            to   w-wat-org-fas          .
           move      t-org-pro            to   w-wat-org-pro          .
           move      t-dst-sna            to   w-wat-dst-sna          .
           move      t-dst-azi            to   w-wat-dst-azi          .
           move      t-dst-ute            to   w-wat-dst-ute          .
           move      t-dst-ter            to   w-wat-dst-ter          .
           move      t-dst-psw            to   w-wat-dst-psw          .
           move      t-dst-iad            to   w-wat-dst-iad          .
           move      t-dst-msg            to   w-wat-dst-msg          .
           move      w-wat                to   g-rec                  .
      *              *-------------------------------------------------*
      *              * Scrittura                                       *
      *              *-------------------------------------------------*
           perform   wrt-rec-pkf-000      thru wrt-rec-pkf-999        .
       out-are-wat-999.
           exit.

      *    *===========================================================*
      *    * Input area w-wat                                          *
      *    *-----------------------------------------------------------*
       inp-are-wat-000.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           perform   rea-rec-pkf-000      thru rea-rec-pkf-999        .
           if        t-sts                not  = e-not-err
                     go to   inp-are-wat-999.
      *              *-------------------------------------------------*
      *              * Decomposizione da record ad area w-wat          *
      *              *-------------------------------------------------*
           move      g-rec                to   w-wat                  .
      *              *-------------------------------------------------*
      *              * Informazioni da area w-wat ad area 't'          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sub-area t-fil                              *
      *                  *---------------------------------------------*
           move      w-wat-fil-nam        to   t-fil-nam              .
           move      w-wat-fil-pat        to   t-fil-pat              .
      *                  *---------------------------------------------*
      *                  * Sub-area t-org                              *
      *                  *---------------------------------------------*
           move      w-wat-org-sna        to   t-org-sna              .
           move      w-wat-org-dat        to   t-org-dat              .
           move      w-wat-org-tim        to   t-org-tim              .
           move      w-wat-org-azi        to   t-org-azi              .
           move      w-wat-org-ute        to   t-org-ute              .
           move      w-wat-org-ter        to   t-org-ter              .
           move      w-wat-org-sap        to   t-org-sap              .
           move      w-wat-org-arg        to   t-org-arg              .
           move      w-wat-org-set        to   t-org-set              .
           move      w-wat-org-fas        to   t-org-fas              .
           move      w-wat-org-pro        to   t-org-pro              .
      *                  *---------------------------------------------*
      *                  * Sub-area t-dst                              *
      *                  *---------------------------------------------*
           move      w-wat-dst-sna        to   t-dst-sna              .
           move      w-wat-dst-azi        to   t-dst-azi              .
           move      w-wat-dst-ute        to   t-dst-ute              .
           move      w-wat-dst-ter        to   t-dst-ter              .
           move      w-wat-dst-psw        to   t-dst-psw              .
           move      w-wat-dst-iad        to   t-dst-iad              .
           move      w-wat-dst-msg        to   t-dst-msg              .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     inp-are-wat-999.
       inp-are-wat-999.
           exit.

      *    *===========================================================*
      *    * Scrittura effettiva record [pkf]                          *
      *    *-----------------------------------------------------------*
       wrt-rec-pkf-000.
      *              *-------------------------------------------------*
      *              * Write                                           *
      *              *-------------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Test su esito operazione di i-o                 *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to wrt-rec-pkf-999.
      *              *-------------------------------------------------*
      *              * Se errori di i-o : comunque fatal error         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di i-o error                      *
      *                  *---------------------------------------------*
           perform   msg-ioe-err-000      thru msg-ioe-err-999        .
      *                  *---------------------------------------------*
      *                  * Status di fatal error in uscita             *
      *                  *---------------------------------------------*
           move      e-fat-err            to   t-sts                  .
      *                  *---------------------------------------------*
      *                  * Chiusura forzata [pkf]                      *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     wrt-rec-pkf-999.
       wrt-rec-pkf-999.
           exit.

      *    *===========================================================*
      *    * Lettura effettiva record [pkf]                            *
      *    *-----------------------------------------------------------*
       rea-rec-pkf-000.
      *              *-------------------------------------------------*
      *              * Get next                                        *
      *              *-------------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Test su esito operazione di i-o                 *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to rea-rec-pkf-999.
           if        g-sts                =    e-end-fil
                     move  e-end-fil      to   t-sts
                     go to rea-rec-pkf-999.
      *              *-------------------------------------------------*
      *              * Se errori di i-o : comunque fatal error         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di i-o error                      *
      *                  *---------------------------------------------*
           perform   msg-ioe-err-000      thru msg-ioe-err-999        .
      *                  *---------------------------------------------*
      *                  * Status di fatal error in uscita             *
      *                  *---------------------------------------------*
           move      e-fat-err            to   t-sts                  .
      *                  *---------------------------------------------*
      *                  * Chiusura forzata [pkf]                      *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mlsfil"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mlsfil"                         .
       rea-rec-pkf-999.
           exit.

      *    *===========================================================*
      *    * Composizione del messaggio di errore alla linea 1 della   *
      *    * zona messaggi t-err in funzione dello status di i-o con-  *
      *    * tenuto in g-sts                                           *
      *    *-----------------------------------------------------------*
       msg-ioe-err-000.
           move      spaces               to   t-e01                  .
           string    "Errore codice '"
                                delimited by   size
                     g-sts      delimited by   size
                     "' su file '"
                                delimited by   size
                     g-pat      delimited by   spaces
                     "'"        delimited by   size
                                          into t-e01                  .
       msg-ioe-err-999.
           exit.
