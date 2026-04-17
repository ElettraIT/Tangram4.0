
Il castelletto come da esempio permette di ottenere una tabella del tipo

qui sotto riportato.






================================================================================
dcc200                    GESTIONE TABELLA SPESE BOLLO   
================================================================================
Codice spesa bollo         : XXX
Tipo di pagamento          :[Xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx]
--------------------------------------------------------------------------------
                 +--------------------------------------------+
                 |        Scaglione          |    Ammontare   |
                 +--------------------------------------------+
                 | fino a L. 99.999.999.999  | 99.999.999.999 |                 
                 | fino a L. 99.999.999.999  | 99.999.999.999 |             
                 | fino a L. 99.999.999.999  | 99.999.999.999 |           
                 | fino a L. 99.999.999.999  | 99.999.999.999 |           
                 | fino a L. 99.999.999.999  | 99.999.999.999 |           
                 | fino a L. 99.999.999.999  | 99.999.999.999 |           
                 | fino a L. 99.999.999.999  | 99.999.999.999 |           
                 | fino a L. 99.999.999.999  | 99.999.999.999 |           
                 | fino a L. 99.999.999.999  | 99.999.999.999 |           
                 | fino a L. 99.999.999.999  | 99.999.999.999 |           
                 +--------------------------------------------+

================================================================================





      *    *===========================================================*
      *    * Work area per accettazione tabella _________              *
      *    *-----------------------------------------------------------*
       01  w-cst.
           05  w-cst-flg-exi              pic  x(01)                  .
           05  w-cst-ctr-rig              pic  9(02)                  .
           05  w-cst-cts-rig              pic  9(02)                  .
           05  w-cst-ctx-rig              pic  9(02)                  .
           05  w-cst-cty-rig              pic  9(02)                  .
           05  w-cst-ctz-rig              pic  9(02)                  .
           05  w-cst-sav-___              pic  9(11)                  .
           05  w-cst-cln-scg              pic  x(80) value
               "                 |                           |          
      -        "      |                 "                             .











      *    *===========================================================*
      *    * Accettazione tabella _________                            *
      *    *-----------------------------------------------------------*
       acc-tab-___-000.
      *              *-------------------------------------------------*
      *              * Test se campo da accettare                      *
      *              *-------------------------------------------------*
           if        w-tes-___-___        =    _____
                     go to acc-tab-___-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 1..10                *
      *              *-------------------------------------------------*
           move      1                    to   w-cst-ctr-rig          .
       acc-tab-___-020.
      *              *-------------------------------------------------*
      *              * __________ (primo campo del castelletto)        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-tes-___-___ 
                    (1, w-cst-ctr-rig)    to   w-cst-sav-___          .
       acc-tab-___-040.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "_"                  to   v-tip                  .
           move      __                   to   v-car                  .
           move      ____                 to   v-dec                  .
           move      ______               to   v-sgn                  .
           move      "__"                 to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      30                   to   v-pos                  .
      *                      *-----------------------------------------*
      *                      * Tasti funzione                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione numero riga + 1      *
      *                          *-------------------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                          *-------------------------------------*
      *                          * Up   : sempre ammesso               *
      *                          *-------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                          *-------------------------------------*
      *                          * Down : sempre ammesso               *
      *                          *-------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                          *-------------------------------------*
      *                          * Insr : sempre ammesso, a meno che   *
      *                          *        non si sia in una riga new   *
      *                          *        oppure manchi spazio per     *
      *                          *        l'inserimento                *
      *                          *-------------------------------------*
           if        w-tes-___-___ (1, 10)
                                          not  = zero
                     go to acc-tab-___-060.
           if        w-cst-ctr-rig        =    10
                     go to acc-tab-___-060.
           if        w-cst-sav-___        =    zero
                     go to acc-tab-___-060.
      *                              *---------------------------------*
      *                              * Abilitazione tasto Insr         *
      *                              *---------------------------------*
           move      "INSR"               to   v-pfk (04)             .
       acc-tab-___-060.
      *                          *-------------------------------------*
      *                          * Do   : sempre ammesso               *
      *                          *-------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                          *-------------------------------------*
      *                          * Remv : sempre ammesso               *
      *                          *-------------------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                          *-------------------------------------*
      *                          * Back : sempre ammesso, a meno che   *
      *                          *        si sia gia' sulla prima riga *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        not  = 1
                     move  "BACK"         to   v-pfk (09)             .
      *                          *-------------------------------------*
      *                          * Tab  : sempre ammesso               *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
      *                      *-----------------------------------------*
      *                      * Valore di accettazione                  *
      *                      *-----------------------------------------*
           move      w-tes-___-___ 
                    (1, w-cst-ctr-rig)    to   v-num                  .
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di accettazione     *
      *                      *-----------------------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to acc-tab-___-300.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-tes
                     go to acc-tab-___-999.
      *                  *---------------------------------------------*
      *                  * Se Delt                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "DELT"
                     move  "X"            to   w-cnt-tus-acc-tes
                     go to acc-tab-___-999.
      *                  *---------------------------------------------*
      *                  * Se premuto un altro tasto funzione non deve *
      *                  * essere avvenuta variazione del campo        *
      *                  *---------------------------------------------*
           if        v-num                not  = w-cst-sav-___
                     go to acc-tab-___-020.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-tab-___-080.
       acc-tab-___-070.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tab-___-800      thru acc-tab-___-809        .
      *                      *-----------------------------------------*
      *                      * Se su riga > 1 : a riga precedente      *
      *                      *-----------------------------------------*
           if        w-cst-ctr-rig        >    1
                     subtract  1          from w-cst-ctr-rig
                     go to     acc-tab-___-020.
      *                      *-----------------------------------------*
      *                      * ALtrimenti : uscita con Up              *
      *                      *-----------------------------------------*
           move      "UP  "               to   v-key                  .
           go to     acc-tab-___-999.
       acc-tab-___-080.
      *                  *---------------------------------------------*
      *                  * Se Down                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-tab-___-160.
       acc-tab-___-100.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tab-___-800      thru acc-tab-___-809        .
      *                      *-----------------------------------------*
      *                      * Se valore precedentemente salvato era a *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           if        w-cst-sav-___        not  = zero
                     go to acc-tab-___-120.
      *                          *-------------------------------------*
      *                          * Se anche dopo il compattamento ci   *
      *                          * si trova su di uno valore a zero :  *
      *                          * uscita                              *
      *                          *-------------------------------------*
           if        w-tes-___-___
                    (1, w-cst-ctr-rig)    =    zero
                     go to acc-tab-___-600.
      *                          *-------------------------------------*
      *                          * Altrimenti si ricicla sulla stessa  *
      *                          * riga, in pratica sulla successiva   *
      *                          *-------------------------------------*
           go to     acc-tab-___-020.
       acc-tab-___-120.
      *                      *-----------------------------------------*
      *                      * Se il valore precedentemente salvato    *
      *                      * era diverso da zero                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se si e' all'ultima riga : uscita   *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        =    10
                     go to acc-tab-___-600.
      *                      *-----------------------------------------*
      *                      * Altrimenti : a riga successiva          *
      *                      *-----------------------------------------*
           add       1                    to   w-cst-ctr-rig          .
           go to     acc-tab-___-020.
       acc-tab-___-160.
      *                  *---------------------------------------------*
      *                  * Se Insr                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-tab-___-180.
      *                      *-----------------------------------------*
      *                      * Inserimento riga in castelletto         *
      *                      *-----------------------------------------*
           perform   acc-tab-___-820      thru acc-tab-___-829        .
      *                      *-----------------------------------------*
      *                      * Riciclo sulla stessa riga               *
      *                      *-----------------------------------------*
           go to     acc-tab-___-020.
       acc-tab-___-180.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tab-___-200.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tab-___-800      thru acc-tab-___-809        .
      *                      *-----------------------------------------*
      *                      * Controllo tabella _________             *
      *                      *-----------------------------------------*
           perform   acc-tab-___-980      thru acc-tab-___-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-tab-___-040.
      *                      *-----------------------------------------*
      *                      * Controllo globali                       *
      *                      *-----------------------------------------*
           perform   cnt-tdo-nok-000      thru cnt-tdo-nok-999        .
      *                      *-----------------------------------------*
      *                      * Test su esito controllo                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se positivo : uscita                *
      *                          *-------------------------------------*
           if        w-cnt-tdo-nok-flg    =    spaces
                     move  "S"            to   w-cnt-tus-acc-tes
                     go to acc-tab-___-999.
      *                          *-------------------------------------*
      *                          * Altrimenti si torna alla reimposta- *
      *                          * zione della stessa riga             *
      *                          *-------------------------------------*
           go to     acc-tab-___-040.
       acc-tab-___-200.
      *                  *---------------------------------------------*
      *                  * Se Remv                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-tab-___-220.
       acc-tab-___-210.
      *                      *-----------------------------------------*
      *                      * Compattamento non controllato           *
      *                      *-----------------------------------------*
           perform   acc-tab-___-810      thru acc-tab-___-819        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione della stessa riga        *
      *                      *-----------------------------------------*
           go to     acc-tab-___-020.
       acc-tab-___-220.
      *                  *---------------------------------------------*
      *                  * Se Prsc                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-tab-___-240.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tab-___-800      thru acc-tab-___-809        .
      *                      *-----------------------------------------*
      *                      * Controllo tabella _________             *
      *                      *-----------------------------------------*
           perform   acc-tab-___-980      thru acc-tab-___-989        .
           if        w-cst-flg-exi        not  = spaces
                     go to acc-tab-___-040.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-tab-___-999.
       acc-tab-___-240.
      *                  *---------------------------------------------*
      *                  * Se Back                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-tab-___-260.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tab-___-800      thru acc-tab-___-809        .
      *                      *-----------------------------------------*
      *                      * Ad impostazione della prima riga        *
      *                      *-----------------------------------------*
           move      1                    to   w-cst-ctr-rig          .
           go to     acc-tab-___-020.
       acc-tab-___-260.
      *                  *---------------------------------------------*
      *                  * Se Tab                                      *
      *                  *---------------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-tab-___-340.
      *                      *-----------------------------------------*
      *                      * Compattamento controllato               *
      *                      *-----------------------------------------*
           perform   acc-tab-___-800      thru acc-tab-___-809        .
      *                      *-----------------------------------------*
      *                      * Se il valore precedentemente salvato    *
      *                      * era a zero                              *
      *                      *-----------------------------------------*
           if        w-cst-sav-___        not  = zero
                     go to acc-tab-___-280.
      *                          *-------------------------------------*
      *                          * Se anche dopo il compattamento ci   *
      *                          * si trova su di un valore a zero :   *
      *                          * uscita                              *
      *                          *-------------------------------------*
           if        w-tes-___-___
                    (1, w-cst-ctr-rig)    =    zero
                     go to acc-tab-___-600.
       acc-tab-___-280.
      *                      *-----------------------------------------*
      *                      * Se il valore precedentemente salvato    *
      *                      * era diverso da zero ci si posiziona     *
      *                      * dopo l'ultima riga                      *
      *                      *-----------------------------------------*
           add       1                    to   w-cst-ctr-rig          .
      *                          *-------------------------------------*
      *                          * Se sono presenti tutte le righe :   *
      *                          * uscita                              *
      *                          *-------------------------------------*
           if        w-cst-ctr-rig        >    10
                     go to acc-tab-___-600.
      *                          *-------------------------------------*
      *                          * Se la riga e' vuota si va ad impo-  *
      *                          * starla                              *
      *                          *-------------------------------------*
           if        w-tes-___-___
                    (1, w-cst-ctr-rig)    =    zero
                     go to acc-tab-___-020.
      *                          *-------------------------------------*
      *                          * Altrimenti si ricicla per trovare   *
      *                          * una riga vuota                      *
      *                          *-------------------------------------*
           go to     acc-tab-___-280.
       acc-tab-___-300.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione valore impostato         *
      *                      *-----------------------------------------*
           move      v-num                to   w-tes-___-___
                                              (1, w-cst-ctr-rig)      .
      *                      *-----------------------------------------*
      *                      * Se campo vuoto : come Down              *
      *                      *-----------------------------------------*
           if        v-num                =    zero
                     go to acc-tab-___-100.
      *                  *---------------------------------------------*
      *                  * Controlli impostazione _____(I campo)___    *
      *                  *---------------------------------------------*







       acc-tab-___-340.
      *                          *-------------------------------------*
      *                          * Accettazione ______ (secondo campo) *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se campo da accettare      *
      *                              *---------------------------------*
           if        w-tes-tau-spb (1)    not  = 2
                     go to acc-tab-___-380.
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "GB"                 to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      48                   to   v-pos                  .
      *                              *---------------------------------*
      *                              * Tasti funzione                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione nr riga + 1  *
      *                                  *-----------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                                  *-----------------------------*
      *                                  * Down : sempre ammesso       *
      *                                  *-----------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                                  *-----------------------------*
      *                                  * Remv : sempre ammesso       *
      *                                  *-----------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                                  *-----------------------------*
      *                                  * Valore di accettazione      *
      *                                  *-----------------------------*
           move      w-tes-tbe-asc 
                    (1, w-cst-ctr-rig)    to   v-num                  .
      *                                  *-----------------------------*
      *                                  * Richiamo sub. accettazione  *
      *                                  *-----------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                          *-------------------------------------*
      *                          * Se Return                           *
      *                          *-------------------------------------*
           if        v-key                =    spaces
                     go to acc-tab-___-360.
      *                          *-------------------------------------*
      *                          * Se Exit o Remv                      *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "REMV"
                     go to acc-tab-___-210.
       acc-tab-___-360.
      *                          *-------------------------------------*
      *                          * Se Return o Down                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore impostato in campo di    *
      *                              * destinazione                    *
      *                              *---------------------------------*
           move      v-num                to   w-tes-tbe-asc
                                              (1, w-cst-ctr-rig)      .
      *                              *---------------------------------*
      *                              * Ammontare a zero non ammesso    *
      *                              * a meno che non sia la prima     *
      *                              * riga                            *
      *                              *---------------------------------*
           if        w-cst-ctr-rig        =    1
                     go to acc-tab-___-380.
           if        w-tes-tbe-asc
                    (1, w-cst-ctr-rig)    =    zero
                     go to acc-tab-___-340.
       acc-tab-___-380.
      *                          *-------------------------------------*
      *                          * Accettazione percentuale            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se campo da accettare      *
      *                              *---------------------------------*
           if        w-tes-tau-spb (1)    not  = 3
                     go to acc-tab-___-580.
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      58                   to   v-pos                  .
      *                              *---------------------------------*
      *                              * Tasti funzione                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione nr riga + 1  *
      *                                  *-----------------------------*
           add       1
                     w-cst-ctr-rig      giving w-cst-cts-rig          .
      *                                  *-----------------------------*
      *                                  * Up   : sempre ammesso       *
      *                                  *-----------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                                  *-----------------------------*
      *                                  * Down : sempre ammesso       *
      *                                  *-----------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                                  *-----------------------------*
      *                                  * Remv : sempre ammesso       *
      *                                  *-----------------------------*
           move      "REMV"               to   v-pfk (06)             .
      *                                  *-----------------------------*
      *                                  * Valore di accettazione      *
      *                                  *-----------------------------*
           move      w-tes-tbe-psc 
                    (1, w-cst-ctr-rig)    to   v-num                  .
      *                                  *-----------------------------*
      *                                  * Richiamo sub. accettazione  *
      *                                  *-----------------------------*
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                          *-------------------------------------*
      *                          * Se Return                           *
      *                          *-------------------------------------*
           if        v-key                =    spaces
                     go to acc-tab-___-400.
      *                          *-------------------------------------*
      *                          * Se Exit o Remv                      *
      *                          *-------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "REMV"
                     go to acc-tab-___-210.
      *                          *-------------------------------------*
      *                          * Se Up                               *
      *                          *-------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-tab-___-340.
       acc-tab-___-400.
      *                          *-------------------------------------*
      *                          * Se Return o Down                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore impostato in campo di    *
      *                              * destinazione                    *
      *                              *---------------------------------*
           move      v-num                to   w-tes-tbe-psc
                                              (1, w-cst-ctr-rig)      .
      *                              *---------------------------------*
      *                              * Percentuale a zero non ammessa  *
      *                              * a meno che non sia la prima     *
      *                              * riga                            *
      *                              *---------------------------------*
           if        w-cst-ctr-rig        =    1
                     go to acc-tab-___-580.
           if        w-tes-tbe-psc
                    (1, w-cst-ctr-rig)    =    zero
                     go to acc-tab-___-380.
      *                              *---------------------------------*
      *                              * Ad incremento numero riga       *
      *                              *---------------------------------*
           go to     acc-tab-___-580.
       acc-tab-___-580.
      *              *-------------------------------------------------*
      *              * Incremento numero riga                          *
      *              *-------------------------------------------------*
           add       1                    to   w-cst-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Se fine righe : uscita                      *
      *                  *---------------------------------------------*
           if        w-cst-ctr-rig        >    10
                     go to acc-tab-___-600.
      *                  *---------------------------------------------*
      *                  * Altrimenti : a prossimo scaglione           *
      *                  *---------------------------------------------*
           go to     acc-tab-___-020.
       acc-tab-___-600.
      *              *-------------------------------------------------*
      *              * Eventuale eliminazione prompt per scaglione     *
      *              *-------------------------------------------------*
           if        w-tes-___-___
                    (1, w-cst-ctr-rig)    not  = zero
                     go to acc-tab-___-620.
           if        w-cst-ctr-rig        >    10
                     go to acc-tab-___-620.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      20                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tab-___-620.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-tab-___-999.
       acc-tab-___-800.
      *              *-------------------------------------------------*
      *              * Subroutine interna di compattamento controllato *
      *              * tabella : solo se lo scaglione e' a zero        *
      *              *-------------------------------------------------*
           if        w-tes-___-___
                    (1, w-cst-ctr-rig)    not  = zero
                     go to acc-tab-___-809.
           if        w-cst-ctr-rig        =    10
                     go to acc-tab-___-809.
           if        w-tes-___-___
                    (1, w-cst-cts-rig)    =    zero
                     go to acc-tab-___-809.
           perform   acc-tab-___-810      thru acc-tab-___-819        .
       acc-tab-___-809.
           exit.
       acc-tab-___-810.
      *              *-------------------------------------------------*
      *              * Subroutine interna di compattamento righe della *
      *              * tabella escludendo la riga w-cst-ctr-rig        *
      *              *-------------------------------------------------*
           move      w-cst-ctr-rig        to   w-cst-ctx-rig          .
           add       1
                     w-cst-ctx-rig    giving   w-cst-cty-rig          .
       acc-tab-___-811.
           if        w-cst-ctx-rig        =    10
                     move  zero           to   w-tes-tbe-asc (1, 10)
                     move  zero           to   w-tes-___-___ (1, 10)
                     move  zero           to   w-tes-tbe-psc (1, 10)
                     go to acc-tab-___-812.
           move      w-tes-tbe-spb
                    (1, w-cst-cty-rig)    to   w-tes-tbe-spb
                                              (1, w-cst-ctx-rig)      .
           add       1                    to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
           go to     acc-tab-___-811.
       acc-tab-___-812.
           move      w-cst-ctr-rig        to   w-cst-ctx-rig          .
           add       09                   to   w-cst-ctx-rig          .
           add       1
                     w-cst-ctx-rig    giving   w-cst-cty-rig          .
       acc-tab-___-813.
           if        w-cst-ctx-rig        =    19
                     go to acc-tab-___-814.
      *              *-------------------------------------------------*
      *              * Taglio della linea video                        *
      *              *-------------------------------------------------*
           move      "FL"                 to   v-ope                  .
           move      w-cst-cty-rig        to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      w-cst-ctx-rig        to   v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video vuota         *
      *              *-------------------------------------------------*
           if        v-alf                =    w-cst-cln-scg
                     go to acc-tab-___-815.
           add       1                    to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
           go to     acc-tab-___-813.
       acc-tab-___-814.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cst-cln-scg        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tab-___-815.
       acc-tab-___-819.
           exit.
       acc-tab-___-820.
      *              *-------------------------------------------------*
      *              * Subroutine interna di inserimento della riga    *
      *              * numero w-cst-ctr-rig nella tabella _________    *
      *              *-------------------------------------------------*
           move      9                    to   w-cst-cty-rig          .
       acc-tab-___-821.
           if        w-tes-___-___
                    (1, w-cst-cty-rig)    =    zero
                     subtract  1          from w-cst-cty-rig
                     go to     acc-tab-___-821.
           move      w-cst-cty-rig        to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
       acc-tab-___-822.
           move      w-tes-tbe-spb
                    (1, w-cst-ctx-rig)    to   w-tes-tbe-spb
                                              (1, w-cst-cty-rig)      .
           if        w-cst-ctx-rig        not  = w-cst-ctr-rig
                     subtract  1          from w-cst-ctx-rig
                     subtract  1          from w-cst-cty-rig
                     go to acc-tab-___-822.
           move      zero                 to   w-tes-___-___
                                              (1, w-cst-ctr-rig)      .
           move      zero                 to   w-tes-tbe-asc
                                              (1, w-cst-ctr-rig)      .
           move      zero                 to   w-tes-tbe-psc
                                              (1, w-cst-ctr-rig)      .
           move      9                    to   w-cst-cty-rig          .
       acc-tab-___-823.
           if        w-cst-cty-rig        =    w-cst-ctr-rig
                     go to     acc-tab-___-824.
           if        w-tes-___-___
                    (1, w-cst-cty-rig)    =    zero
                     subtract  1          from w-cst-cty-rig
                     go to     acc-tab-___-823.
       acc-tab-___-824.
           move      w-cst-cty-rig        to   w-cst-ctx-rig          .
           add       1                    to   w-cst-cty-rig          .
       acc-tab-___-825.
      *              *-------------------------------------------------*
      *              * Taglio della linea video                        *
      *              *-------------------------------------------------*
           move      "FL"                 to   v-ope                  .
           add       09
                     w-cst-ctx-rig      giving v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       09
                     w-cst-cty-rig      giving v-lin                  .
           move      01                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-cst-ctx-rig        not  = w-cst-ctr-rig
                     subtract  1          from w-cst-ctx-rig
                     subtract  1          from w-cst-cty-rig
                     go to acc-tab-___-825.
      *              *-------------------------------------------------*
      *              * Visualizzazione della linea video vuota         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cst-cln-scg        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tab-___-829.
           exit.
       acc-tab-___-980.
      *              *-------------------------------------------------*
      *              * Subroutine interna di esecuzione operazioni per *
      *              * fine impostazione tabella _________             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione status di uscita            *
      *                  *---------------------------------------------*
           move      spaces               to   w-cst-flg-exi          .
      *                  *---------------------------------------------*
      *                  * Controllo che almeno una riga sia presente  *
      *                  *---------------------------------------------*
           if        w-tes-___-___ (1, 1) =    zero
                     move  "#"            to   w-cst-flg-exi
                     go to acc-tab-___-989.
       acc-tab-___-989.
           exit.
       acc-tab-___-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione tabella _________                         *
      *    *-----------------------------------------------------------*
       vis-tbs-spb-000.
      *              *-------------------------------------------------*
      *              * Test se campo da visualizzare                   *
      *              *-------------------------------------------------*
           if        w-tes-tip-pag        =    zero
                     go to vis-tbs-spb-999.
           if        w-tes-tau-spb (1)    =    1
                     go to vis-tbs-spb-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 1..10            *
      *                  *---------------------------------------------*
           move      zero                 to   w-cst-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Ciclo 1..10                                 *
      *                  *---------------------------------------------*
       vis-tbs-spb-100.
           add       1                    to   w-cst-ctr-rig          .
           if        w-cst-ctr-rig        >    10
                     go to  vis-tbs-spb-999.
           if        w-tes-___-___ 
                    (1, w-cst-ctr-rig)    =    zero and
                     w-tes-tbe-asc 
                    (1, w-cst-ctr-rig)    =    zero and
                     w-tes-tbe-psc 
                    (1, w-cst-ctr-rig)    =    zero
                     go to  vis-tbs-spb-999.
      *                      *-----------------------------------------*
      *                      * Prompt per scaglione                    *
      *                      *-----------------------------------------*
           perform   pmt-acc-scg-000      thru pmt-acc-scg-999        .
      *                      *-----------------------------------------*
      *                      * Scaglione                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      30                   to   v-pos                  .
           move      w-tes-___-___
                    (1, w-cst-ctr-rig)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ammontare                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se campo da visualizzare       *
      *                          *-------------------------------------*
           if        w-tes-tau-spb (1)    not  = 2
                     go to vis-tbs-spb-200.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           if        w-tes-tbe-asc
                    (1, w-cst-ctr-rig)    =    zero
                     move  "9"            to   v-edm
           else      move  "GB"           to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      48                   to   v-pos                  .
           move      w-tes-tbe-asc
                    (1, w-cst-ctr-rig)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tbs-spb-200.
      *                      *-----------------------------------------*
      *                      * Percentuale                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se campo da visualizzare       *
      *                          *-------------------------------------*
           if        w-tes-tau-spb (1)    not  = 3
                     go to vis-tbs-spb-300.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           if        w-tes-tbe-psc
                    (1, w-cst-ctr-rig)    =    zero
                     move  "9"            to   v-edm
           else      move  "BD"           to   v-edm                  .
           add       09
                     w-cst-ctr-rig      giving v-lin                  .
           move      58                   to   v-pos                  .
           move      w-tes-tbe-psc
                    (1, w-cst-ctr-rig)    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tbs-spb-300.
      *                      *-----------------------------------------*
      *                      * Riciclo a riga castelletto successiva   *
      *                      *-----------------------------------------*
           go to     vis-tbs-spb-100.
       vis-tbs-spb-999.
           exit.

