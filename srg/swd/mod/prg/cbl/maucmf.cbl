       Identification Division.
       Program-Id.                                 maucmf             .
      *================================================================*
      *                                                                *
      *    Modulo per il trattamento del file [auc] per i sottosiste-  *
      *    mi :                                                        *
      *                                                                *
      *    - Aziende                                                   *
      *    - Utenti                                                    *
      *    - Comandi                                                   *
      *    - Menu'                                                     *
      *    - Filtri di conversione standard                            *
      *    - Nodi IP                                                   *
      *                                                                *
      * ============================================================== *
      *                     Versione originale:    001 del 01/01/92    *
      *                       Ultima revisione:    NdK del 17/02/13    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Apertura modulo trattamento file [auc]             *
      *                                                                *
      *             Input  : j-ope = "OP"                              *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              "##"      : errore grave          *
      *                                                                *
      *                      j-msg = messaggio di errore gia' emesso   *
      *                              se errore grave                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Chiusura modulo trattamento file [auc]             *
      *                                                                *
      *             Input  : j-ope = "CL"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Cancel      Test se modulo trattamento file [auc] sottoponibi- *
      *             le a cancellazione                                 *
      *                                                                *
      *             Input  : j-ope = "X?"                              *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : modulo cancellabile   *
      *                              "##"      : modulo non cancell.   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Norm        Normalizzazione record file [auc]                  *
      *                                                                *
      *             Input  : j-ope = "NO"                              *
      *                                                                *
      *                      j-tre = "PSG" per personalizzaz. generali *
      *                              "AZI" per azienda                 *
      *                              "UTE" per utente                  *
      *                              "CMD" per comando                 *
      *                              "MNU" per menu'                   *
      *                              "PNX" per pathnames sostitutivi   *
      *                              "FCS" per filtri conversione      *
      *                              "DPS" per definizioni personaliz. *
      *                              "DRF" per definizioni referenze   *
      *                              "BAK" per procedure di backup     *
      *                              "NIP" per nodi IP                 *
      *                                                                *
      *             Output : j-dat = dati record normalizzati          *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Start       Start su file [auc]                                *
      *                                                                *
      *             Input  : j-ope = "ST"                              *
      *                                                                *
      *                      j-tre = "AZI" per azienda                 *
      *                              "UTE" per utente                  *
      *                              "CMD" per comando                 *
      *                              "MNU" per menu'                   *
      *                              "PNX" per pathnames sostitutivi   *
      *                              "FCS" per filtri conversione      *
      *                              "DPS" per definizioni personaliz. *
      *                              "DRF" per definizioni referenze   *
      *                              "BAK" per procedure di backup     *
      *                              "NIP" per nodi IP                 *
      *                                                                *
      *                      j-kre = sigla  azienda          per "AZI" *
      *                              codice utente           per "UTE" *
      *                              sigla  comando          per "CMD" *
      *                              sigla  menu'            per "MNU" *
      *                              pathname originale      per "PNX" *
      *                              codice del filtro       per "FCS" *
      *                              codice personalizzaz.   per "DPS" *
      *                              codice referenza        per "DRF" *
      *                              codice procedura        per "BAK" *
      *                              codice nodo             per "NIP" *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-not-fnd : start errata          *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-not-fnd        *
      *                                                                *
      *             Nota   : La start viene comunque eseguita con il   *
      *                      tipo di confronto 'Not less'              *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Read        Read next su file [auc]                            *
      * Next                                                           *
      *             Input  : j-ope = "RN"                              *
      *                                                                *
      *                      j-tre = "AZI" per azienda                 *
      *                              "UTE" per utente                  *
      *                              "CMD" per comando                 *
      *                              "MNU" per menu'                   *
      *                              "PNX" per pathnames sostitutivi   *
      *                              "FCS" per filtri conversione      *
      *                              "DPS" per definizioni personaliz. *
      *                              "DRF" per definizioni referenze   *
      *                              "BAK" per procedure di backup     *
      *                              "NIP" per nodi IP                 *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-end-fil : fine fisica o logica  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-end-fil        *
      *                                                                *
      *             Nota   : La read next viene comunque eseguita sen- *
      *                      za lock                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Read        Lettura record file [auc]                          *
      *                                                                *
      *             Input  : j-ope = "RD"                              *
      *                                                                *
      *                      j-tre = "PSG" per personalizzaz. generali *
      *                              "AZI" per azienda                 *
      *                              "UTE" per utente                  *
      *                              "CMD" per comando                 *
      *                              "MNU" per menu'                   *
      *                              "PNX" per pathnames sostitutivi   *
      *                              "FCS" per filtri conversione      *
      *                              "DPS" per definizioni personaliz. *
      *                              "DRF" per definizioni referenze   *
      *                              "BAK" per procedure di backup     *
      *                              "NIP" per nodi IP                 *
      *                                                                *
      *                      j-kre = sigla  azienda          per "AZI" *
      *                              codice utente           per "UTE" *
      *                              sigla  comando          per "CMD" *
      *                              sigla  menu'            per "MNU" *
      *                              pathname originale      per "PNX" *
      *                              codice del filtro       per "FCS" *
      *                              codice personalizzaz.   per "DPS" *
      *                              codice referenza        per "DRF" *
      *                              codice procedura        per "BAK" *
      *                              codice nodo             per "NIP" *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-not-fnd : record non esistente  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-not-fnd        *
      *                                                                *
      *                      j-dat = dati record                       *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Put         Scrittura record file [auc]                        *
      *                                                                *
      *             Input  : j-ope = "PT"                              *
      *                                                                *
      *                      j-tre = "PSG" per personalizzaz. generali *
      *                              "AZI" per azienda                 *
      *                              "UTE" per utente                  *
      *                              "CMD" per comando                 *
      *                              "MNU" per menu'                   *
      *                              "PNX" per pathnames sostitutivi   *
      *                              "FCS" per filtri conversione      *
      *                              "DPS" per definizioni personaliz. *
      *                              "DRF" per definizioni referenze   *
      *                              "BAK" per procedure di backup     *
      *                              "NIP" per nodi IP                 *
      *                                                                *
      *                      j-kre = sigla  azienda          per "AZI" *
      *                              codice utente           per "UTE" *
      *                              sigla  comando          per "CMD" *
      *                              sigla  menu'            per "MNU" *
      *                              pathname originale      per "PNX" *
      *                              codice del filtro       per "FCS" *
      *                              codice personalizzaz.   per "DPS" *
      *                              codice referenza        per "DRF" *
      *                              codice procedura        per "BAK" *
      *                              codice nodo             per "NIP" *
      *                                                                *
      *                      j-dat = dati record                       *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-dup-key : record gia' esistente *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-dup-key        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Update      Riscrittura record file [auc]                      *
      *                                                                *
      *             Input  : j-ope = "UP"                              *
      *                                                                *
      *                      j-tre = "PSG" per personalizzaz. generali *
      *                              "AZI" per azienda                 *
      *                              "UTE" per utente                  *
      *                              "CMD" per comando                 *
      *                              "MNU" per menu'                   *
      *                              "PNX" per pathnames sostitutivi   *
      *                              "FCS" per filtri conversione      *
      *                              "DPS" per definizioni personaliz. *
      *                              "DRF" per definizioni referenze   *
      *                              "BAK" per procedure di backup     *
      *                              "NIP" per nodi IP                 *
      *                                                                *
      *                      j-kre = sigla  azienda          per "AZI" *
      *                              codice utente           per "UTE" *
      *                              sigla  comando          per "CMD" *
      *                              sigla  menu'            per "MNU" *
      *                              pathname originale      per "PNX" *
      *                              codice del filtro       per "FCS" *
      *                              codice personalizzaz.   per "DPS" *
      *                              codice referenza        per "DRF" *
      *                              codice procedura        per "BAK" *
      *                              codice nodo             per "NIP" *
      *                                                                *
      *                      j-dat = dati record                       *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              e-not-fnd : record non esistente  *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
      *                              - non  emesso se e-not-fnd        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Delete      Delete record file [auc]                           *
      *                                                                *
      *             Input  : j-ope = "DE"                              *
      *                                                                *
      *                      j-tre = "PSG" per personalizzaz. generali *
      *                              "AZI" per azienda                 *
      *                              "UTE" per utente                  *
      *                              "CMD" per comando                 *
      *                              "MNU" per menu'                   *
      *                              "PNX" per pathnames sostitutivi   *
      *                              "FCS" per filtri conversione      *
      *                              "DPS" per definizioni personaliz. *
      *                              "DRF" per definizioni referenze   *
      *                              "BAK" per procedure di backup     *
      *                              "NIP" per nodi IP                 *
      *                                                                *
      *                      j-kre = sigla  azienda          per "AZI" *
      *                              codice utente           per "UTE" *
      *                              sigla  comando          per "CMD" *
      *                              sigla  menu'            per "MNU" *
      *                              pathname originale      per "PNX" *
      *                              codice del filtro       per "FCS" *
      *                              codice personalizzaz.   per "DPS" *
      *                              codice referenza        per "DRF" *
      *                              codice procedura        per "BAK" *
      *                              codice nodo             per "NIP" *
      *                                                                *
      *                      j-dat = dati record                       *
      *                                                                *
      *             Output : j-rsc = Return status code                *
      *                              spaces    : nessun errore         *
      *                              "##"      : errore grave di i-o   *
      *                                                                *
      *                      j-msg = messaggio di errore               *
      *                              - gia' emesso se errore di i-o    *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [auc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  auc   assign to disk       f-auc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is auc-key
                             file status  is            f-auc-sts     .
       

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [auc]                                    *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/fls/rec/rfauc"                          .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work area per trattamento [auc] riguardante il cobol      *
      *    *-----------------------------------------------------------*
       01  f-auc.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-auc-nam                  pic  x(04) value "auc "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-auc-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-auc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .
       
      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Work area per contatore di Open in corso                  *
      *    *-----------------------------------------------------------*
       01  w-opn.
      *        *-------------------------------------------------------*
      *        * COntatore Open in corso                               *
      *        *-------------------------------------------------------*
           05  w-opn-ctr                  pic  9(03) value zero       .

      *    *===========================================================*
      *    * Work per records di [auc] 'azi'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucazi0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'ute'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucute0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'cmd'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wauccmd0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'mnu'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucmnu0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'psg'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucpsg0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'pnx'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucpnx0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'fcs'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucfcs0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'dps'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucdps0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'drf'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucdrf0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'bak'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucbak0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'nip'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucnip0.cpw"                   .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Work per composizione messaggio di errore             *
      *        *-------------------------------------------------------*
           05  w-msg                      pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Contatore                                             *
      *        *-------------------------------------------------------*
           05  w-ctr                      pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per trattamento chiave                         *
      *        *-------------------------------------------------------*
           05  w-tre                      pic  x(04)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio chiave con ridefinizioni                  *
      *        *-------------------------------------------------------*
           05  w-sav-kre                  pic  x(40)                  .
           05  w-sav-kre-azi redefines
               w-sav-kre.
               10  w-sav-cod-azi          pic  x(04)                  .
               10  filler                 pic  x(36)                  .
           05  w-sav-kre-ute redefines
               w-sav-kre.
               10  w-sav-cod-ute          pic  x(08)                  .
               10  filler                 pic  x(32)                  .
           05  w-sav-kre-cmd redefines
               w-sav-kre.
               10  w-sav-cod-cmd          pic  x(06)                  .
               10  filler                 pic  x(34)                  .
           05  w-sav-kre-mnu redefines
               w-sav-kre.
               10  w-sav-cod-mnu          pic  x(06)                  .
               10  w-sav-npg-mnu          pic  9(02)                  .
               10  filler                 pic  x(32)                  .
           05  w-sav-kre-pnx redefines
               w-sav-kre.
               10  w-sav-pat-org          pic  x(40)                  .
           05  w-sav-kre-fcs redefines
               w-sav-kre.
               10  w-sav-cod-fcs          pic  x(40)                  .
           05  w-sav-kre-dps redefines
               w-sav-kre.
               10  w-sav-cod-dps          pic  x(32)                  .
               10  filler                 pic  x(08)                  .
           05  w-sav-kre-drf redefines
               w-sav-kre.
               10  w-sav-cod-drf          pic  x(32)                  .
               10  filler                 pic  x(08)                  .
           05  w-sav-kre-bak redefines
               w-sav-kre.
               10  w-sav-cod-bak          pic  x(12)                  .
               10  filler                 pic  x(28)                  .
           05  w-sav-kre-nip redefines
               w-sav-kre.
               10  w-sav-cod-nip          pic  x(08)                  .
               10  filler                 pic  x(32)                  .

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
      *    * Area di comunicazione per modulo                "mpslct"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "maucmf"  *
      *    *-----------------------------------------------------------*
       01  j.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  j-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo record                                           *
      *        *-------------------------------------------------------*
           05  j-tre                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Chiave record                                         *
      *        *-------------------------------------------------------*
           05  j-kre                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Dati record                                           *
      *        *-------------------------------------------------------*
           05  j-dat.
               10  j-chr occurs 2048      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        *-------------------------------------------------------*
           05  j-rsc                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return message                                        *
      *        *-------------------------------------------------------*
           05  j-msg                      pic  x(80)                  .

      ******************************************************************
       Procedure Division                using j                      .
      ******************************************************************

      *================================================================*
      *       Declaratives                                             *
      *----------------------------------------------------------------*
       Declaratives.
       Decl Section.
           Use after standard error procedure on auc.
       decl-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-auc-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-auc                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-auc-sts            to   e-sts                  .
       End Declaratives.

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       Main Section.
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione return status code e message    *
      *              *-------------------------------------------------*
           move      spaces               to   j-rsc
                                               j-msg                  .
      *              *-------------------------------------------------*
      *              * Test su tipo operazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura modulo                             *
      *                  *---------------------------------------------*
           if        j-ope                =    "OP"
                     perform opn-mod-000  thru opn-mod-999
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           else if   j-ope                =    "CL"
                     perform cls-mod-000  thru cls-mod-999
      *                  *---------------------------------------------*
      *                  * Test se modulo cancellabile                 *
      *                  *---------------------------------------------*
           else if   j-ope                =    "X?"
                     perform tst-cnc-000  thru tst-cnc-999
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           else if   j-ope                =    "NO"
                     perform nor-rec-000  thru nor-rec-999
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           else if   j-ope                =    "ST"
                     perform str-rec-000  thru str-rec-999
      *                  *---------------------------------------------*
      *                  * Read next                                   *
      *                  *---------------------------------------------*
           else if   j-ope                =    "RN"
                     perform nxt-rec-000  thru nxt-rec-999
      *                  *---------------------------------------------*
      *                  * Read record                                 *
      *                  *---------------------------------------------*
           else if   j-ope                =    "RD"
                     perform rea-rec-000  thru rea-rec-999
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           else if   j-ope                =    "PT"
                     perform put-rec-000  thru put-rec-999
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           else if   j-ope                =    "UP"
                     perform upd-rec-000  thru upd-rec-999
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           else if   j-ope                =    "DE"
                     perform del-rec-000  thru del-rec-999            .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Apertura modulo                                           *
      *    *-----------------------------------------------------------*
       opn-mod-000.
      *              *-------------------------------------------------*
      *              * Se contatore di Open in corso maggiore di zero  *
      *              * Open non necessaria                             *
      *              *-------------------------------------------------*
           if        w-opn-ctr            not  = zero
                     go to opn-mod-400.
      *              *-------------------------------------------------*
      *              * Open i-o [auc]                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione error-code                  *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [auc]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fpx"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "auc"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-auc-pat              .
      *                  *---------------------------------------------*
      *                  * Operazione di Open                          *
      *                  *---------------------------------------------*
           open      i-o    auc                                       .
      *              *-------------------------------------------------*
      *              * Test se errori in open i-o                      *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to opn-mod-500.
       opn-mod-400.
      *              *-------------------------------------------------*
      *              * Incremento contatore di Open in corso           *
      *              *-------------------------------------------------*
           add       1                    to   w-opn-ctr              .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     opn-mod-999.
       opn-mod-500.
      *              *-------------------------------------------------*
      *              * Se errori in open i-o                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      "##"                 to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      "Sottosistema di configurazione non disponibile !"
                                          to   w-msg                  .
           move      w-msg                to   j-msg                  .
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-msg                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di presa visione                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      33                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in on                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione OK di presa visione            *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
       opn-mod-600.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT" or
                     v-key                =    "DO  "
                     go to opn-mod-800.
           if        v-alf                not  = "OK"
                     go to opn-mod-600.
       opn-mod-800.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       opn-mod-999.
           exit.

      *    *===========================================================*
      *    * Chiusura modulo                                           *
      *    *-----------------------------------------------------------*
       cls-mod-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore di Open in corso           *
      *              *-------------------------------------------------*
           subtract  1                    from w-opn-ctr              .
      *              *-------------------------------------------------*
      *              * Se contatore di Open in corso a zero : Close    *
      *              *-------------------------------------------------*
           if        w-opn-ctr            not  = zero
                     go to cls-mod-999.
           close     auc                                              .
       cls-mod-999.
           exit.

      *    *===========================================================*
      *    * Test se modulo cancellabile                               *
      *    *-----------------------------------------------------------*
       tst-cnc-000.
      *              *-------------------------------------------------*
      *              * Se il numero di Open in corso e' zero il modulo *
      *              * e' cancellabile, altrimenti non lo e'           *
      *              *-------------------------------------------------*
           if        w-opn-ctr            =    zero
                     move  spaces         to   j-rsc
           else      move  "##"           to   j-rsc                  .
       tst-cnc-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record                                    *
      *    *-----------------------------------------------------------*
       nor-rec-000.
      *              *-------------------------------------------------*
      *              * Test su tipo record                             *
      *              *-------------------------------------------------*
           if        j-tre                =    "PSG "
                     perform nor-psg-000  thru nor-psg-999
                     move    w-psg        to   j-dat
           else if   j-tre                =    "AZI "
                     perform nor-azi-000  thru nor-azi-999
                     move    w-azi        to   j-dat
           else if   j-tre                =    "UTE "
                     perform nor-ute-000  thru nor-ute-999
                     move    w-ute        to   j-dat
           else if   j-tre                =    "CMD "
                     perform nor-cmd-000  thru nor-cmd-999
                     move    w-cmd        to   j-dat
           else if   j-tre                =    "MNU "
                     perform nor-mnu-000  thru nor-mnu-999
                     move    w-mnu        to   j-dat
           else if   j-tre                =    "PNX "
                     perform nor-pnx-000  thru nor-pnx-999
                     move    w-pnx        to   j-dat
           else if   j-tre                =    "FCS "
                     perform nor-fcs-000  thru nor-fcs-999
                     move    w-fcs        to   j-dat
           else if   j-tre                =    "DPS "
                     perform nor-dps-000  thru nor-dps-999
                     move    w-dps        to   j-dat
           else if   j-tre                =    "DRF "
                     perform nor-drf-000  thru nor-drf-999
                     move    w-drf        to   j-dat
           else if   j-tre                =    "BAK "
                     perform nor-bak-000  thru nor-bak-999
                     move    w-bak        to   j-dat
           else if   j-tre                =    "NIP "
                     perform nor-nip-000  thru nor-nip-999
                     move    w-bak        to   j-dat                  .
       nor-rec-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "psg "                     *
      *    *-----------------------------------------------------------*
       nor-psg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           perform   nor-psg-auc-000      thru nor-psg-auc-999        .
       nor-psg-999.
           exit.

      *    *===========================================================*
      *    * Work per normalizzazione record di [auc] 'psg'            *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucpsg0.cps"                   .

      *    *===========================================================*
      *    * Normalizzazione record di tipo "azi "                     *
      *    *-----------------------------------------------------------*
       nor-azi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-azi-cod-azi
                                               w-azi-des-azi
                                               w-azi-pwd-azi          .
           move      zero                 to   w-azi-sts-azi          .
           move      spaces               to   w-azi-pnb-fls
                                               w-azi-pnb-prf
                                               w-azi-pnb-spl          .
           move      spaces               to   w-azi-pns-tbl          .
           move      spaces               to   w-azi-sgl-vlt          .
           move      spaces               to   w-azi-des-vlt          .
           move      spaces               to   w-azi-din-vlt          .
           move      zero                 to   w-azi-dec-vlt          .
           move      spaces               to   w-azi-tdc-vlt          .
           move      zero                 to   w-azi-cdc-vlt          .
       nor-azi-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "ute "                     *
      *    *-----------------------------------------------------------*
       nor-ute-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-ute-cod-ute
                                               w-ute-des-ute          .
           move      zero                 to   w-ute-tip-ute          .
           move      spaces               to   w-ute-pwd-ute          .
           move      zero                 to   w-ute-liv-pro
                                               w-ute-sts-ute          .
           move      spaces               to   w-ute-mnu-ini          .
           move      zero                 to   w-ute-num-azi          .
           move      spaces               to   w-ute-tbl-azi          .
           move      zero                 to   w-ute-num-abl          .
           move      spaces               to   w-ute-tbl-abl          .
           move      spaces               to   w-ute-bkg-ute          .
           move      zero                 to   w-ute-tse-cst          .
           move      spaces               to   w-ute-cst-ass          .
           move      zero                 to   w-ute-amc-def          .
           move      zero                 to   w-ute-ali-def          .
           move      spaces               to   w-ute-csl-aau          .
           move      spaces               to   w-ute-aps-aau          .
           move      zero                 to   w-ute-dat-agg          .
           move      zero                 to   w-ute-dat-uti          .
           move      zero                 to   w-ute-dat-scd          .
           move      spaces               to   w-ute-frs-aid          .
           move      spaces               to   w-ute-eml-ute          .
           move      spaces               to   w-ute-ipa-ute          .
           move      spaces               to   w-ute-def-tst          .
           move      spaces               to   w-ute-alx-exp          .
       nor-ute-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "cmd "                     *
      *    *-----------------------------------------------------------*
       nor-cmd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-cmd-cod-cmd
                                               w-cmd-des-cmd          .
           move      zero                 to   w-cmd-tip-cmd          .
           move      spaces               to   w-cmd-pth-cmd
                                               w-cmd-sis-cmd
                                               w-cmd-are-cmd
                                               w-cmd-set-cmd
                                               w-cmd-fas-cmd
                                               w-cmd-prg-cmd
                                               w-cmd-mnu-rie
                                               w-cmd-pwd-cmd          .
           move      zero                 to   w-cmd-tip-ute
                                               w-cmd-liv-pro
                                               w-cmd-sts-cmd
                                               w-cmd-tip-tsk          .
           move      spaces               to   w-cmd-alx-exp          .
       nor-cmd-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "mnu "                     *
      *    *-----------------------------------------------------------*
       nor-mnu-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-mnu-cod-mnu          .
           move      zero                 to   w-mnu-npg-mnu          .
           move      zero                 to   w-ctr                  .
       nor-mnu-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    18
                     go to nor-mnu-999.
           move      zero                 to   w-mnu-mkp-mnu (w-ctr)  .
           move      spaces               to   w-mnu-mkc-mnu (w-ctr)  .
           move      spaces               to   w-mnu-lin-mnu (w-ctr)  .
           go to     nor-mnu-100.
       nor-mnu-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "pnx "                     *
      *    *-----------------------------------------------------------*
       nor-pnx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-pnx-pat-org
                                               w-pnx-pat-sos          .
       nor-pnx-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "fcs "                     *
      *    *-----------------------------------------------------------*
       nor-fcs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-fcs-cod-fcs          .
           move      spaces               to   w-fcs-des-fcs          .
           move      spaces               to   w-fcs-pwd-fcs          .
           move      zero                 to   w-fcs-num-pcv          .
       nor-fcs-100.
           add       1                    to   w-fcs-num-pcv          .
           if        w-fcs-num-pcv        >    36
                     go to nor-fcs-200.
           move      zero                 to   w-fcs-tip-ope
                                              (w-fcs-num-pcv)         .
           move      spaces               to   w-fcs-tip-cmp
                                              (w-fcs-num-pcv)         .
           move      zero                 to   w-fcs-car-cmp
                                              (w-fcs-num-pcv)         .
           move      zero                 to   w-fcs-dec-cmp
                                              (w-fcs-num-pcv)         .
           move      spaces               to   w-fcs-sgn-cmp
                                              (w-fcs-num-pcv)         .
           move      spaces               to   w-fcs-trs-cmp
                                              (w-fcs-num-pcv)         .
           move      zero                 to   w-fcs-dsp-cmp
                                              (w-fcs-num-pcv)         .
           move      spaces               to   w-fcs-val-cmp
                                              (w-fcs-num-pcv)         .
           go to     nor-fcs-100.
       nor-fcs-200.
           move      zero                 to   w-fcs-num-pcv          .
       nor-fcs-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "dps "                     *
      *    *-----------------------------------------------------------*
       nor-dps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-dps-cod-dps          .
           move      spaces               to   w-dps-des-dps          .
           move      spaces               to   w-dps-tip-dps          .
           move      zero                 to   w-dps-car-dps          .
           move      zero                 to   w-dps-dec-dps          .
           move      spaces               to   w-dps-sgn-dps          .
           move      spaces               to   w-dps-som-dps          .
           move      spaces               to   w-dps-alx-dps          .
           move      spaces               to   w-dps-com-dps          .
       nor-dps-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "drf "                     *
      *    *-----------------------------------------------------------*
       nor-drf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-drf-cod-drf          .
           move      spaces               to   w-drf-des-drf          .
           move      spaces               to   w-drf-tip-drf          .
           move      zero                 to   w-drf-car-drf          .
           move      zero                 to   w-drf-dec-drf          .
           move      spaces               to   w-drf-sgn-drf          .
           move      spaces               to   w-drf-som-drf          .
           move      spaces               to   w-drf-alx-drf          .
           move      spaces               to   w-drf-com-drf          .
       nor-drf-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "bak "                     *
      *    *-----------------------------------------------------------*
       nor-bak-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-bak-cod-bak          .
           move      zero                 to   w-bak-tip-bak          .
           move      spaces               to   w-bak-des-bak          .
           move      zero                 to   w-bak-tut-abl          .
           move      spaces               to   w-bak-pwd-bak          .
           move      zero                 to   w-bak-sts-bak          .
           move      zero                 to   w-bak-tip-dev          .
           move      spaces               to   w-bak-pdd-uns          .
           move      spaces               to   w-bak-pth-dir          .
           move      spaces               to   w-bak-alx-exp          .
           move      zero                 to   w-bak-num-edd          .
           move      spaces               to   w-bak-tbl-edd          .
       nor-bak-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record di tipo "nip "                     *
      *    *-----------------------------------------------------------*
       nor-nip-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-nip-cod-nip          .
           move      spaces               to   w-nip-des-nip          .
           move      zero                 to   w-nip-tip-nip          .
           move      spaces               to   w-nip-ute-nip          .
           move      zero                 to   w-nip-ver-nip          .
           move      spaces               to   w-nip-iip-nip          .
           move      spaces               to   w-nip-nom-nip          .
           move      spaces               to   w-nip-num-nip          .
           move      spaces               to   w-nip-ubi-nip          .
           move      spaces               to   w-nip-oss-nip          .
           move      zero                 to   w-nip-agg-nip          .
           move      spaces               to   w-nip-mtr-nip          .
           move      spaces               to   w-nip-cpc-nip          .
           move      spaces               to   w-nip-usr-nip          .
           move      spaces               to   w-nip-pwd-nip          .
           move      spaces               to   w-nip-not-nip          .
       nor-nip-999.
           exit.

      *    *===========================================================*
      *    * Start                                                     *
      *    *-----------------------------------------------------------*
       str-rec-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           if        j-tre                =    "PSG "
                     go to str-rec-999.
      *
           move      04                   to   w-all-str-lun          .
           move      j-tre                to   w-all-str-alf          .
           perform   all-str-low-000      thru all-str-low-999        .
           move      w-all-str-alf        to   auc-tre                .
      *
           move      j-kre                to   auc-kre                .
      *              *-------------------------------------------------*
      *              * Start                                           *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           start     auc    key not less
                            auc-key
                            invalid key
                            go to   str-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in lettura record                     *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to str-rec-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     str-rec-999.
       str-rec-400.
      *              *-------------------------------------------------*
      *              * Se start errata                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-not-fnd            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      "Start errata su [auc] !"
                                          to   j-msg                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
       str-rec-999.
           exit.

      *    *===========================================================*
      *    * Read next                                                 *
      *    *-----------------------------------------------------------*
       nxt-rec-000.
      *              *-------------------------------------------------*
      *              * Test su tipo record                             *
      *              *-------------------------------------------------*
           if        j-tre                =    "PSG "
                     go to nxt-rec-999.
       nxt-rec-100.
      *              *-------------------------------------------------*
      *              * Read next                                       *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      auc  next with no lock
                               at end
                               go to nxt-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in lettura record                     *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to nxt-rec-600.
           if        e-sts                =    e-use-err
                     go to nxt-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     nxt-rec-999.
       nxt-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     nxt-rec-100.
       nxt-rec-400.
      *              *-------------------------------------------------*
      *              * Se at end                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-end-fil            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
       nxt-rec-490.
           move      "Fine file [auc] !"  to   j-msg                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     nxt-rec-999.
       nxt-rec-600.
      *              *-------------------------------------------------*
      *              * Se non at end                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se fine logica del file                *
      *                  *---------------------------------------------*
           move      04                   to   w-all-str-lun          .
           move      j-tre                to   w-all-str-alf          .
           perform   all-str-low-000      thru all-str-low-999        .
           move      w-all-str-alf        to   w-tre                  .
      *
           if        auc-tre              =    w-tre
                     go to nxt-rec-800
           else      go to nxt-rec-400.
       nxt-rec-800.
      *                  *---------------------------------------------*
      *                  * Spostamento area dati in area di link       *
      *                  *---------------------------------------------*
           move      auc-kre              to   j-kre                  .
           move      auc-dat              to   j-dat                  .
       nxt-rec-999.
           exit.

      *    *===========================================================*
      *    * Read record                                               *
      *    *-----------------------------------------------------------*
       rea-rec-000.
      *              *-------------------------------------------------*
      *              * Salvataggio chiave                              *
      *              *-------------------------------------------------*
           move      j-kre                to   w-sav-kre              .
       rea-rec-100.
      *              *-------------------------------------------------*
      *              * Preparazione chiave                             *
      *              *-------------------------------------------------*
           move      04                   to   w-all-str-lun          .
           move      j-tre                to   w-all-str-alf          .
           perform   all-str-low-000      thru all-str-low-999        .
           move      w-all-str-alf        to   auc-tre                .
      *
           if        j-tre                =    "PSG "
                     move  spaces         to   auc-kre
           else      move  j-kre          to   auc-kre                .
      *              *-------------------------------------------------*
      *              * Lettura record senza lock                       *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      auc    with no lock
                            invalid key
                            go to   rea-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in lettura record                     *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to rea-rec-600.
           if        e-sts                =    e-use-err
                     go to rea-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-rec-999.
       rea-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     rea-rec-100.
       rea-rec-400.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-not-fnd            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-msg                  .
           if        j-tre                =    "PSG "
                     go to rea-rec-405
           else if   j-tre                =    "AZI "
                     go to rea-rec-410
           else if   j-tre                =    "UTE "
                     go to rea-rec-415
           else if   j-tre                =    "CMD "
                     go to rea-rec-420
           else if   j-tre                =    "MNU "
                     go to rea-rec-425
           else if   j-tre                =    "PNX "
                     go to rea-rec-430
           else if   j-tre                =    "FCS "
                     go to rea-rec-440
           else if   j-tre                =    "DPS "
                     go to rea-rec-445
           else if   j-tre                =    "DRF "
                     go to rea-rec-450
           else if   j-tre                =    "BAK "
                     go to rea-rec-455
           else if   j-tre                =    "NIP "
                     go to rea-rec-460
           else      go to rea-rec-490.
       rea-rec-405.
           move      "Personalizzazioni generali non esistenti !"
                                          to   w-msg                  .
           go to     rea-rec-500.
       rea-rec-410.
           string    "Azienda '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-415.
           string    "Utente '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-420.
           string    "Comando '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-425.
           string    "Menu '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-430.
           string    "Pathname sost. '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-440.
           string    "Filtro di conversione '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-445.
           string    "Personalizzazione '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-450.
           string    "Referenza '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-455.
           string    "Backup '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-460.
           string    "Nodo di rete '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     rea-rec-500.
       rea-rec-490.
           move      "Record non esistente !"
                                          to   w-msg                  .
       rea-rec-500.
           move      w-msg                to   j-msg                  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione area dati in work           *
      *                  *---------------------------------------------*
           perform   nor-rec-000          thru nor-rec-999            .
      *                  *---------------------------------------------*
      *                  * Ricostruzione chiave e dati                 *
      *                  *---------------------------------------------*
           if        j-tre                =    "PSG "
                     move  w-psg          to   j-dat
           else if   j-tre                =    "AZI "
                     move  w-sav-cod-azi  to   w-azi-cod-azi
                     move  w-azi          to   j-dat
           else if   j-tre                =    "UTE "
                     move  w-sav-cod-ute  to   w-ute-cod-ute
                     move  w-ute          to   j-dat
           else if   j-tre                =    "CMD "
                     move  w-sav-cod-cmd  to   w-cmd-cod-cmd
                     move  w-cmd          to   j-dat
           else if   j-tre                =    "MNU "
                     move  w-sav-cod-mnu  to   w-mnu-cod-mnu
                     move  w-sav-npg-mnu  to   w-mnu-npg-mnu
                     move  w-mnu          to   j-dat
           else if   j-tre                =    "PNX "
                     move  w-sav-pat-org  to   w-pnx-pat-org
                     move  w-pnx          to   j-dat
           else if   j-tre                =    "FCS "
                     move  w-sav-cod-fcs  to   w-fcs-cod-fcs
                     move  w-fcs          to   j-dat
           else if   j-tre                =    "DPS "
                     move  w-sav-cod-dps  to   w-dps-cod-dps
                     move  w-dps          to   j-dat
           else if   j-tre                =    "DRF "
                     move  w-sav-cod-drf  to   w-drf-cod-drf
                     move  w-drf          to   j-dat
           else if   j-tre                =    "BAK "
                     move  w-sav-cod-bak  to   w-bak-cod-bak
                     move  w-bak          to   j-dat
           else if   j-tre                =    "NIP "
                     move  w-sav-cod-nip  to   w-nip-cod-nip
                     move  w-nip          to   j-dat                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-rec-999.
       rea-rec-600.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Spostamento area dati in area di link       *
      *                  *---------------------------------------------*
           move      auc-dat              to   j-dat                  .
       rea-rec-999.
           exit.

      *    *===========================================================*
      *    * Put record                                                *
      *    *-----------------------------------------------------------*
       put-rec-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave e dati                      *
      *              *-------------------------------------------------*
           move      04                   to   w-all-str-lun          .
           move      j-tre                to   w-all-str-alf          .
           perform   all-str-low-000      thru all-str-low-999        .
           move      w-all-str-alf        to   auc-tre                .
      *
           if        j-tre                =    "PSG "
                     move  spaces         to   auc-kre
           else      move  j-kre          to   auc-kre                .
      *
           move      j-dat                to   auc-dat                .
      *              *-------------------------------------------------*
      *              * Scrittura record                                *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           write     auc-rec invalid key
                             go to   put-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in scrittura record                   *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to put-rec-999.
           if        e-sts                =    e-use-err
                     go to put-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     put-rec-999.
       put-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     put-rec-000.
       put-rec-400.
      *              *-------------------------------------------------*
      *              * Se record gia' esistente                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-dup-key            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-msg                  .
           if        j-tre                =    "PSG "
                     go to put-rec-405
           else if   j-tre                =    "AZI "
                     go to put-rec-410
           else if   j-tre                =    "UTE "
                     go to put-rec-415
           else if   j-tre                =    "CMD "
                     go to put-rec-420
           else if   j-tre                =    "MNU "
                     go to put-rec-425
           else if   j-tre                =    "PNX "
                     go to put-rec-430
           else if   j-tre                =    "FCS "
                     go to put-rec-440
           else if   j-tre                =    "DPS "
                     go to put-rec-445
           else if   j-tre                =    "DRF "
                     go to put-rec-450
           else if   j-tre                =    "BAK "
                     go to put-rec-455
           else if   j-tre                =    "NIP "
                     go to put-rec-460
           else      go to put-rec-490.
       put-rec-405.
           move      "Personalizzazioni generali gia' esistenti !"
                                          to   w-msg                  .
           go to     put-rec-500.
       put-rec-410.
           string    "Azienda '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-415.
           string    "Utente '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-420.
           string    "Comando '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-425.
           string    "Menu '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-430.
           string    "Pathname sost. '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-440.
           string    "Filtro di conversione '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-445.
           string    "Personalizzazione '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-450.
           string    "Referenza '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-455.
           string    "Backup '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-460.
           string    "Nodo di rete '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' gia' esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     put-rec-500.
       put-rec-490.
           move      "Record gia' esistente !"
                                          to   w-msg                  .
       put-rec-500.
           move      w-msg                to   j-msg                  .
       put-rec-999.
           exit.

      *    *===========================================================*
      *    * Update record                                             *
      *    *-----------------------------------------------------------*
       upd-rec-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave e dati                      *
      *              *-------------------------------------------------*
           move      04                   to   w-all-str-lun          .
           move      j-tre                to   w-all-str-alf          .
           perform   all-str-low-000      thru all-str-low-999        .
           move      w-all-str-alf        to   auc-tre                .
      *
           if        j-tre                =    "PSG "
                     move  spaces         to   auc-kre
           else      move  j-kre          to   auc-kre                .
      *
           move      j-dat                to   auc-dat                .
      *              *-------------------------------------------------*
      *              * Riscrittura record                              *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           rewrite   auc-rec invalid key
                             go to   upd-rec-400.
      *              *-------------------------------------------------*
      *              * Se errori in riscritture record                 *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to upd-rec-999.
           if        e-sts                =    e-use-err
                     go to upd-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     upd-rec-999.
       upd-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     upd-rec-000.
       upd-rec-400.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Return status code                          *
      *                  *---------------------------------------------*
           move      e-not-fnd            to   j-rsc                  .
      *                  *---------------------------------------------*
      *                  * Return message                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-msg                  .
           if        j-tre                =    "PSG "
                     go to upd-rec-405
           else if   j-tre                =    "AZI "
                     go to upd-rec-410
           else if   j-tre                =    "UTE "
                     go to upd-rec-415
           else if   j-tre                =    "CMD "
                     go to upd-rec-420
           else if   j-tre                =    "MNU "
                     go to upd-rec-425
           else if   j-tre                =    "PNX "
                     go to upd-rec-430
           else if   j-tre                =    "FCS "
                     go to upd-rec-440
           else if   j-tre                =    "DPS "
                     go to upd-rec-445
           else if   j-tre                =    "DRF "
                     go to upd-rec-450
           else if   j-tre                =    "BAK "
                     go to upd-rec-455
           else if   j-tre                =    "NIP "
                     go to upd-rec-460
           else      go to upd-rec-490.
       upd-rec-405.
           move      "Personalizzazioni generali non esistenti !"
                                          to   w-msg                  .
           go to     upd-rec-500.
       upd-rec-410.
           string    "Azienda '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-415.
           string    "Utente '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-420.
           string    "Comando '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-425.
           string    "Menu '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-430.
           string    "Pathname sost. '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-440.
           string    "Filtro di conversione '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-445.
           string    "Personalizzazione '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-450.
           string    "Referenza '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-455.
           string    "Backup '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-460.
           string    "Nodo di rete '"
                                delimited by   size
                     j-kre
                                delimited by   spaces
                     "' non esistente !"
                                delimited by   size
                                          into w-msg                  .
           go to     upd-rec-500.
       upd-rec-490.
           move      "Record non esistente !"
                                          to   w-msg                  .
       upd-rec-500.
           move      w-msg                to   j-msg                  .
       upd-rec-999.
           exit.

      *    *===========================================================*
      *    * Delete record                                             *
      *    *-----------------------------------------------------------*
       del-rec-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave e dati                      *
      *              *-------------------------------------------------*
           move      04                   to   w-all-str-lun          .
           move      j-tre                to   w-all-str-alf          .
           perform   all-str-low-000      thru all-str-low-999        .
           move      w-all-str-alf        to   auc-tre                .
      *
           if        j-tre                =    "PSG "
                     move  spaces         to   auc-kre
           else      move  j-kre          to   auc-kre                .
      *
           move      j-dat                to   auc-dat                .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           delete    auc     invalid key
                             go to   del-rec-999.
      *              *-------------------------------------------------*
      *              * Se errori in delete record                      *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to del-rec-999.
           if        e-sts                =    e-use-err
                     go to del-rec-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione del messaggio di errore     *
      *                  *---------------------------------------------*
           perform   msg-err-000          thru msg-err-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     del-rec-999.
       del-rec-200.
      *              *-------------------------------------------------*
      *              * Se record locked                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Wait di un secondo                          *
      *                  *---------------------------------------------*
           perform   wai-t1s-000          thru wai-t1s-999            .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura record                 *
      *                  *---------------------------------------------*
           go to     del-rec-000.
       del-rec-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione del messaggio di errore a centro video e  *
      *    * chiusura del modulo                                       *
      *    *-----------------------------------------------------------*
       msg-err-000.
      *              *-------------------------------------------------*
      *              * Return status code                              *
      *              *-------------------------------------------------*
           move      "##"                 to   j-rsc                  .
      *              *-------------------------------------------------*
      *              * Return message                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-msg                  .
           string    "Errore di i-o codice '"
                                delimited by   size
                     e-sts
                                delimited by   size
                     "' su file '"
                                delimited by   size
                     f-auc-pat
                                delimited by   spaces
                     "' !"
                                delimited by   size
                                          into w-msg                  .
           move      w-msg                to   j-msg                  .
      *              *-------------------------------------------------*
      *              * Emissione messaggio ed accettazione OK          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-msg                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dicitura di presa visione                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      33                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in on                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione OK di presa visione            *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
       msg-err-300.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT" or
                     v-key                =    "DO  "
                     go to msg-err-400.
           if        v-alf                not  = "OK"
                     go to msg-err-300.
       msg-err-400.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       msg-err-999.
           exit.

      *    *===========================================================*
      *    * Wait di un secondo                                        *
      *    *-----------------------------------------------------------*
       wai-t1s-000.
           call      "swd/mod/prg/obj/mwait0"                         .
       wai-t1s-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

