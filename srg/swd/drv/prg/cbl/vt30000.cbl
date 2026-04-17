       Identification Division.
       Program-Id.                                 vt30000            .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    drv                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/01/01    *
      *                       Ultima revisione:    NdK del 28/11/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Implementazione gestione video per terminali tipo DEC VT300    *
      *                                                                *
      * per Acucobol.                                                  *
      *                                                                *
      * Nota : Questo driver vale anche per la console di MS-DOS.      *
      *                                                                *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/01/01    *
      *                       Ultima revisione:    NdK del 23/03/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *                                                                *
      * =========================[ UNIX ]============================= *
      *                                                                *
      *  ATTENZIONE: GLI EXCEPTION-CODES SONO RICONOSCIUTI E TRATTATI  *
      *              ANCHE NEI MODULI 'MOPSYS'                         *
      *                                                                *
      *  Tasto logico Xpg  Exception Code  Sigla Termcap  Tasto fisico *
      *  ----------------  --------------  -------------  ------------ *
      *                                                                *
      *  [1]               01              k1             PF1          *
      *                                                                *
      *  [2]               02              k2             PF2          *
      *                                                                *
      *  [3]               03              k3             PF3          *
      *                                                                *
      *  [4]               04              k4             PF4          *
      *                                                                *
      *  libero            05              k5             nessuno      *
      *                                                                *
      *  RFSH              06              k6             F6           *
      *                                                                *
      *  COPY              07              k7             F7           *
      *                                                                *
      *  PAST              08              k8             F8           *
      *                                                                *
      *  EXPD              09              k9             F9           *
      *                                                                *
      *  APND              10              k0             F10          *
      *                                                                *
      *  ICHR              11              K1             F11          *
      *                                                                *
      *  DCHR              12              K2             F12          *
      *                                                                *
      *  libero            13              K3             F13          *
      *                                                                *
      *  BACK              14              K4             F14          *
      *                                                                *
      *  HELP              15              K5             HELP         *
      *                                                                *
      *  DO                16              K6             DO           *
      *                                                                *
      *  DELT              17              K7             F17          *
      *                                                                *
      *  PRNT              18              K8             F18          *
      *                                                                *
      *  libero            19              K9             F19          *
      *                                                                *
      *  EXIT              20              K0             F20          *
      *                                                                *
      *  FIND              41              KF             FIND         *
      *                                                                *
      *  INSR              42              kA             INSERT HERE  *
      *                                                                *
      *  REMV              43              kL             REMOVE       *
      *                                                                *
      *  SLCT              44              KM             Select       *
      *                                                                *
      *  PRSC              45              kP             PREV SCREEN  *
      *                                                                *
      *  NXSC              46              kN             NEXT SCREEN  *
      *                                                                *
      *  UP                51              ku             UP           *
      *                                                                *
      *  DOWN              52              kd             DOWN         *
      *                                                                *
      *  RGHT              53              kr             RIGHT        *
      *                                                                *
      *  LEFT              54              kl             LEFT         *
      *                                                                *
      *  LEFT              54              ZB             BACKSPACE    *
      *                                                                *
      *  RTRN              61              ^M             RETURN       *
      *                                                                *
      *  TAB               62              ^I             TAB          *
      *                                                                *
      *  SHCP              65              ^P             Ctrl-P       *
      *                                                                *
      *                 -------------------------------                *
      *                 Set-Up del terminale DEC VT-300                *
      *                 -------------------------------                *
      *                                                                *
      * Display Set-Up                                                 *
      * --------------                                                 *
      *                                                                *
      * - 80 Columns                                                   *
      * - Interpret Controls                                           *
      * - Auto Wrap                                                    *
      * - Jump Scroll                                                  *
      * - Light-Text, Dark Screen                                      *
      * - Cursor                                                       *
      * - Underline Cursor Style                                       *
      * - No Status Display                                            *
      *                                                                *
      *                                                                *
      * General Set-Up                                                 *
      * --------------                                                 *
      *                                                                *
      * - VT300 Mode, 7 Bit Control                                    *
      * - VT300 ID                                                     *
      * - User Defined Keys Unlocked                                   *
      * - User Features Unlocked                                       *
      * - 8-Bit Characters                                             *
      * - Numeric Keypad                                               *
      * - Normal Cursor Keys                                           *
      * - No New Line                                                  *
      * - UPSS DEC Supplemental                                        *
      *                                                                *
      *                                                                *
      * Communications Set-Up                                          *
      * ---------------------                                          *
      *                                                                *
      * - Transmit=9600 (oppure 19200 oppure .....)                    *
      * - Receive=Transmit                                             *
      * - XOFF at 64                                                   *
      * - 7 Bits, Even Parity                                          *
      * - 1 Stop Bit                                                   *
      * - No Local Echo                                                *
      * - DEC-423, Data Leads Only (oppure RS-232, Data Leads Only)    *
      * - Disconnect, 2 s Delay                                        *
      * - Unlimited Transmit                                           *
      * - Auto Answerback                                              *
      * - Answerback=                                                  *
      * - Not Concealed                                                *
      *                                                                *
      *                                                                *
      * Printer Set-Up                                                 *
      * --------------                                                 *
      *                                                                *
      * - nessun parametro significativo                               *
      *                                                                *
      *                                                                *
      * Keyboard Set-Up                                                *
      * ---------------                                                *
      *                                                                *
      * - Typewriters keys                                             *
      * - Caps Lock                                                    *
      * - Auto Repeat                                                  *
      * - Keyclick                                                     *
      * - No Margin Bell                                               *
      * - No warning Bell                                              *
      * - No Break                                                     *
      * - No Compose                                                   *
      * - <X] Delete                                                   *
      * - ,, and .. Keys                                               *
      * - <> Key                                                       *
      * - `~ Key                                                       *
      *                                                                *
      *                                                                *
      * Tab Set-Up                                                     *
      * ----------                                                     *
      *                                                                *
      * - nessun parametro significativo                               *
      *                                                                *
      *                --------------------------------                *
      *                Terminal Capabilities per VT-300                *
      *                --------------------------------                *
      *                                                                *
      *     vt300:\                                                    *
      *            :co#80:\                                            *
      *            :li#25:\                                            *
      *            :k1=\EOP:\                                          *
      *            :k2=\EOQ:\                                          *
      *            :k3=\EOR:\                                          *
      *            :k4=\EOS:\                                          *
      *            :k6=\E[17~:\                                        *
      *            :k7=\E[18~:\                                        *
      *            :k8=\E[19~:\                                        *
      *            :k9=\E[20~:\                                        *
      *            :k0=\E[21~:\                                        *
      *            :K1=\E[23~:\                                        *
      *            :K2=\E[24~:\                                        *
      *            :K3=\E[25~:\                                        *
      *            :K4=\E[26~:\                                        *
      *            :K5=\E[28~:\                                        *
      *            :K6=\E[29~:\                                        *
      *            :K7=\E[31~:\                                        *
      *            :K8=\E[32~:\                                        *
      *            :K9=\E[33~:\                                        *
      *            :K0=\E[34~:\                                        *
      *            :KF=\E[1~:\                                         *
      *            :kA=\E[2~:\                                         *
      *            :kL=\E[3~:\                                         *
      *            :KM=\E[4~:\                                         *
      *            :kP=\E[5~:\                                         *
      *            :kN=\E[6~:\                                         *
      *            :ku=\E[A:\                                          *
      *            :kd=\E[B:\                                          *
      *            :kr=\E[C:\                                          *
      *            :kl=\E[D:\                                          *
      *            :cl=\E[;H\E[2J:\                                    *
      *            :cm=\E[%i%d;%dH:\                                   *
      *            :ce=\E[K:\                                          *
      *            :cd=\E[J:\                                          *
      *            :nd=\E[C:\                                          *
      *            :DL:\                                               *
      *            :nd=\E[C:\                                          *
      *            :RV=\E[7m:\                                         *
      *            :HI=\E[0;1m:\                                       *
      *            :LO=\E[m:\                                          *
      *            :BL=\E[5m:\                                         *
      *            :UL=\E[4m:\                                         *
      *            :RB=\E[5;7m:\                                       *
      *            :RU=\E[4;7m:\                                       *
      *            :EP=\E[5i:\                                         *
      *            :DP=\E[4i:\                                         *
      *            :al=\E[L:\                                          *
      *            :dl=\E[M:\                                          *
      *            :GO=\E(0:\                                          *
      *            :GF=\E(B:\                                          *
      *            :GM=qxlkmjvtwun:\                                   *
      *            :W3=\E[?3h:\                                        *
      *            :W8=\E[?3l:\                                        *
      *            :vi=\E[?25l:\                                       *
      *            :ve=\E[?25h:\                                       *
      *                                                                *
      *                                                                *
      * ==========================MS-DOS============================== *
      *                                                                *
      *  Tasto logico Xpg  Exception Code  Sigla Termcap  Tasto fisico *
      *  ----------------  --------------  -------------  ------------ *
      *                                                                *
      *  [1]               01              k1             F1           *
      *                                                                *
      *  [2]               02              k2             F2           *
      *                                                                *
      *  [3]               03              k3             F3           *
      *                                                                *
      *  [4]               04              k4             F4           *
      *                                                                *
      *  RFSH              06              K3             Shift-F3     *
      *                                                                *
      *  COPY              07              K1             Shift-F1     *
      *                                                                *
      *  PAST              08              K2             Shift-F2     *
      *                                                                *
      *  EXPD              09              k6             F6           *
      *                                                                *
      *  APND              10              k5             F5           *
      *                                                                *
      *  ICHR              11              K6             Shift-F6     *
      *                                                                *
      *  DCHR              12              K7             Shift-F7     *
      *                                                                *
      *  BACK              14              kB             Shift-TAB    *
      *                                                                *
      *  HELP              15              k7             F7           *
      *                                                                *
      *  DO                16              k8             F8           *
      *                                                                *
      *  DELT              17              k0             F10          *
      *                                                                *
      *  PRNT              18              k9             F9           *
      *                                                                *
      *  EXIT              20              ^[             ESC          *
      *                                                                *
      *  FIND              41              KE             END          *
      *                                                                *
      *  INSR              42              KI             INS          *
      *                                                                *
      *  REMV              43              KX             DEL          *
      *                                                                *
      *  SLCT              44              kh             HOME         *
      *                                                                *
      *  PRSC              45              kP             PG-UP        *
      *                                                                *
      *  NXSC              46              kN             PG-DN        *
      *                                                                *
      *  UP                51              ku             UP           *
      *                                                                *
      *  DOWN              52              kd             DOWN         *
      *                                                                *
      *  RGHT              53              kr             RIGHT        *
      *                                                                *
      *  LEFT              54              kl             LEFT         *
      *                                                                *
      *  LEFT              54              ZB             BACKSPACE    *
      *                                                                *
      *  RTRN              61              ^M             RETURN       *
      *                                                                *
      *  TAB               62              ^I             TAB          *
      *                                                                *
      *  SHCP              65              ^P             Ctrl-P       *
      *                                                                *
      *                 ------------------------------                 *
      *                 Set-Up della console di MS-DOS                 *
      *                 ------------------------------                 *
      *                                                                *
      * Nessun Setup da eseguire                                       *
      *                                                                *
      *                --------------------------------                *
      *                Terminal Capabilities per VT-300                *
      *                --------------------------------                *
      *                                                                *
      * Nessun file di terminal capabilities necessario                *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [xvi]                                        *
      *    *-----------------------------------------------------------*
           select optional   xvi   assign to disk       f-xvi-pat
                             organization is relative
                             access  mode is random
                             relative key is f-xvi-key
                             file  status is f-xvi-sts                .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [xvi]                                    *
      *    *-----------------------------------------------------------*
       fd  xvi  label record standard.
       01  xvi-rec.
           05  xvi-mod                    pic  9(01)                  .
           05  xvi-dat.
               10  xvi-lin occurs 24.
                   15  xvi-pos occurs 132.
                       20  xvi-chr        pic  x(01)                  .
           
      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .
       
      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Numero linea                                          *
      *        *-------------------------------------------------------*
           05  w-lin                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero posizione                                      *
      *        *-------------------------------------------------------*
           05  w-pos                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Size                                                  *
      *        *-------------------------------------------------------*
           05  w-siz                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Exception code da istruzione 'accept'                 *
      *        *-------------------------------------------------------*
           05  w-exc                      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio carattere attuale per eventuale ripristi- *
      *        * no in caso di Escape 'manuale'                        *
      *        *-------------------------------------------------------*
           05  w-exs                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore/indice su caratteri                         *
      *        *-------------------------------------------------------*
           05  w-ccr                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di accettazione alfanumerica                     *
      *        *  - Spaces : Normale                                   *
      *        *  - C      : Per campo di tipo check-box               *
      *        *-------------------------------------------------------*
           05  w-cta                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di accettazione campo espanso                    *
      *        *  - Spaces : Normale                                   *
      *        *  - C      : Per campo di tipo check-box               *
      *        *-------------------------------------------------------*
           05  w-cte                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Field                                                 *
      *        *-------------------------------------------------------*
           05  w-fld.
               10  w-cho occurs 133.
                   15  w-chr              pic  x(01)                  .
                   15  w-chn redefines
                       w-chr              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Indicatore di primo carattere in accettazione         *
      *        *-------------------------------------------------------*
           05  w-frs                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Indicatore di almeno una variazione in accettazione   *
      *        *-------------------------------------------------------*
           05  w-auv                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Caratteri in uppercase                                *
      *        *-------------------------------------------------------*
           05  w-upp.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upr redefines w-upp.
               10  w-upc occurs 26        pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Caratteri in lowercase                                *
      *        *-------------------------------------------------------*
           05  w-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-lor redefines w-low.
               10  w-loc occurs 26        pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore/indice per operazioni upper/lowercase       *
      *        *-------------------------------------------------------*
           05  w-ulc                      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di Insert Character On/Off                    *
      *        *-------------------------------------------------------*
           05  w-ich                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per box                                        *
      *        *-------------------------------------------------------*
           05  w-box.
      *            *---------------------------------------------------*
      *            * Coordinate Box di comodo                          *
      *            *---------------------------------------------------*
               10  w-box-lin              pic  9(03)                  .
               10  w-box-pos              pic  9(03)                  .
               10  w-box-lto              pic  9(03)                  .
               10  w-box-pto              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio coordinate Box                        *
      *            *---------------------------------------------------*
               10  w-box-sli              pic  9(03)                  .
               10  w-box-spo              pic  9(03)                  .
               10  w-box-slt              pic  9(03)                  .
               10  w-box-spt              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per accettazioni alfanumeriche/espanse/ecc...  *
      *        *-------------------------------------------------------*
           05  w-txe.
      *            *---------------------------------------------------*
      *            * Numero totale caratteri nel campo                 *
      *            *---------------------------------------------------*
               10  w-txe-ntc              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Start line                                        *
      *            *---------------------------------------------------*
               10  w-txe-stl              pic s9(03)                  .
      *            *---------------------------------------------------*
      *            * End line                                          *
      *            *---------------------------------------------------*
               10  w-txe-enl              pic s9(03)                  .
      *            *---------------------------------------------------*
      *            * Start position                                    *
      *            *---------------------------------------------------*
               10  w-txe-stp              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * End position                                      *
      *            *---------------------------------------------------*
               10  w-txe-enp              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Size                                              *
      *            *---------------------------------------------------*
               10  w-txe-siz              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Start index                                       *
      *            *---------------------------------------------------*
               10  w-txe-sti              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * End index                                         *
      *            *---------------------------------------------------*
               10  w-txe-eni              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Ridefinizione area text ed espansa con bordi      *
      *            *---------------------------------------------------*
               10  w-txe-wex.
                   15  w-txe-we1          pic  x(01)                  .
                   15  w-txe-we2.
                       20  w-txe-wec
                               occurs 132 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di campo in stato espanso On/Off          *
      *            *---------------------------------------------------*
               10  w-txe-exp              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio indice di campo espanso               *
      *            *---------------------------------------------------*
               10  w-txe-svn              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio valore letterale di campo espanso     *
      *            *---------------------------------------------------*
               10  w-txe-sva.
                   15  filler occurs 132  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio valore di default di campo espanso    *
      *            *---------------------------------------------------*
               10  w-txe-sav.
                   15  filler occurs 800  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Comodo per trattamento campo espanso              *
      *            *---------------------------------------------------*
               10  w-txe-trt.
                   15  w-txe-cht
                              occurs 800  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo di sottolineatura campo espanso              *
      *            *  - U : Visualizzazione con underscores            *
      *            *  - D : Visualizzazione senza underscores          *
      *            *---------------------------------------------------*
               10  w-txe-tdc              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Indice di linea in accettazione di campo espanso  *
      *            *---------------------------------------------------*
               10  w-txe-ixl              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Indice di posizione in accettazione di campo e-   *
      *            * spanso                                            *
      *            *---------------------------------------------------*
               10  w-txe-ixp              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Indice contatore di comodo per visualizzazione di *
      *            * campo espanso                                     *
      *            *---------------------------------------------------*
               10  w-txe-wnl              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodi locali per calcoli intermedi               *
      *            *---------------------------------------------------*
               10  w-txe-wk1              pic  9(03)                  .
               10  w-txe-wk2              pic  9(03)                  .
               10  w-txe-wk3              pic  9(03)                  .
               10  w-txe-wk4              pic  9(03)                  .
               10  w-txe-wk5              pic  9(03)                  .
               10  w-txe-wt3              pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Comodi locali per Insert e Delete Character       *
      *            *---------------------------------------------------*
               10  w-txe-wkl              pic  9(03)                  .
               10  w-txe-wkp              pic  9(03)                  .
               10  w-txe-wka              pic  9(03)                  .
               10  w-txe-wkz              pic  9(03)                  .
               10  w-txe-wkt              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo locale per unstring insignificante         *
      *            *---------------------------------------------------*
               10  w-txe-wru              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Comodo locale per pointer di unstring             *
      *            *---------------------------------------------------*
               10  w-txe-wpu              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo locale per counter di unstring             *
      *            *---------------------------------------------------*
               10  w-txe-wcu              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Segnale di pre-espansione comunque richiesta      *
      *            *---------------------------------------------------*
               10  w-txe-pee              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di Auto Wrap richiesto                    *
      *            *---------------------------------------------------*
               10  w-txe-awr              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di Auto Wrap attivo                          *
      *            *---------------------------------------------------*
               10  w-txe-awf              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di compressione inibita                   *
      *            *---------------------------------------------------*
               10  w-txe-nco              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di elemento manuale ammesso On/Off        *
      *            *---------------------------------------------------*
               10  w-txe-man              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Indice per navigazione su linee campo espanso     *
      *            *---------------------------------------------------*
               10  w-txe-inx              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio buffer video                          *
      *            *---------------------------------------------------*
               10  w-txe-siv-dat.
                   15  w-txe-siv-lin occurs 24.
                       20  w-txe-siv-pos occurs 132.
                           25  w-txe-siv-chr
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Ridefinizione maschera di editing per campi espa- *
      *            * nsi                                               *
      *            *---------------------------------------------------*
               10  w-txe-med.
                   15  w-txe-mes          pic  9(03)                  .
                   15  w-txe-mey          pic  9(02)                  .
                   15  w-txe-mej          pic  9(02)                  .
                   15  w-txe-mec          pic  x(01)                  .
                   15  w-txe-mef          pic  x(01)                  .
                   15  w-txe-met          pic  x(01)                  .
                   15  w-txe-men          pic  x(09) value "123456789".
                   15  w-txe-men-r redefines
                       w-txe-men.
                       20  w-txe-meh  occurs 09
                                          pic  x(01)                  .
                   15  w-txe-mez          pic  9(03)                  .
                   15  w-txe-mew          pic  9(03)                  .
                   15  w-txe-mex.
                       20  w-txe-mer  occurs 18
                                  indexed by w-txe-mei
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per accettazioni check-box                     *
      *        *-------------------------------------------------------*
           05  w-ckb.
      *            *---------------------------------------------------*
      *            * Salvataggio tipo operazione                       *
      *            *---------------------------------------------------*
               10  w-ckb-ope              pic  x(02)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio tipo campo                            *
      *            *---------------------------------------------------*
               10  w-ckb-tip              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio numero caratteri                      *
      *            *---------------------------------------------------*
               10  w-ckb-car              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio numero linee di testo                 *
      *            *---------------------------------------------------*
               10  w-ckb-ldt              pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio tipo editing                          *
      *            *---------------------------------------------------*
               10  w-ckb-edm              pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio valore serie caratteri                *
      *            *---------------------------------------------------*
               10  w-ckb-alf.
                   15  filler occurs 132  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio valore literals                       *
      *            *---------------------------------------------------*
               10  w-ckb-txt.
                   15  filler occurs 800  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio linea                                 *
      *            *---------------------------------------------------*
               10  w-ckb-lin              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio posizione                             *
      *            *---------------------------------------------------*
               10  w-ckb-pos              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio lista function-keys ammissibili       *
      *            *---------------------------------------------------*
               10  w-ckb-ufk.
                   15  filler occurs 160  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore attuale serie caratteri                    *
      *            *---------------------------------------------------*
               10  w-ckb-vsr.
                   15  filler occurs 132  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per accettazioni campi espansi                 *
      *        *-------------------------------------------------------*
           05  w-esp.
      *            *---------------------------------------------------*
      *            * Valore alfabetico                                 *
      *            *---------------------------------------------------*
               10  w-esp-alf.
                   15  filler occurs 132  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio/ridefinizione anno/mese/giorno   *
      *        *-------------------------------------------------------*
           05  w-amg                      pic  9(06)                  .
           05  w-amx redefines  w-amg.
               10  w-ann                  pic  9(02)                  .
               10  w-mes                  pic  9(02)                  .
               10  w-gio                  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio/ridefinizione della data in for- *
      *        * mato aa.mm.gg, senza secolo                           *
      *        *-------------------------------------------------------*
           05  w-dat                      pic  9(06)                  .
           05  w-dt1 redefines  w-dat.
               10  w-ggg                  pic  9(02)                  .
               10  w-mmm                  pic  9(02)                  .
               10  w-aaa                  pic  9(02)                  .
           05  w-dt2 redefines  w-dat.
               10  w-dtc        occurs 06 pic  9(01)                  .
           05  w-svd                      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per indicatore giorno della settimana attivo     *
      *        *-------------------------------------------------------*
           05  w-dow                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per data editata                                 *
      *        *-------------------------------------------------------*
           05  w-dte.
               10  w-ded                  pic  99/99/99    blank
                                                           when
                                                           zero       .
      *        *-------------------------------------------------------*
      *        * Tabella giorni nel mese                               *
      *        *-------------------------------------------------------*
           05  w-tgm.
               10  filler                 pic  9(02) value 31         .
               10  filler                 pic  9(02) value 28         .
               10  filler                 pic  9(02) value 31         .
               10  filler                 pic  9(02) value 30         .
               10  filler                 pic  9(02) value 31         .
               10  filler                 pic  9(02) value 30         .
               10  filler                 pic  9(02) value 31         .
               10  filler                 pic  9(02) value 31         .
               10  filler                 pic  9(02) value 30         .
               10  filler                 pic  9(02) value 31         .
               10  filler                 pic  9(02) value 30         .
               10  filler                 pic  9(02) value 31         .
           05  w-tgr redefines w-tgm.
               10  w-tgx occurs 12        pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per valore numerico                            *
      *        *-------------------------------------------------------*
           05  w-num                      pic s9(13)v9(05)            .
           05  w-num-cnv                  pic s9(13)v9(02)            .
           05  w-sgn                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per valore progressivo/anno                    *
      *        *-------------------------------------------------------*
           05  w-prt                      pic  9(13)                  .
           05  w-prr redefines
               w-prt.
               10  w-prs                  pic  9(01)                  .
               10  w-pra                  pic  9(02)                  .
               10  w-prx redefines
                   w-pra.
                   15  w-pax occurs 2     pic  9(01)                  .
               10  w-prn                  pic  9(10)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio valore iniziale di un campo di *
      *        * tipo progressivo/anno, non ancora normalizzato        *
      *        *-------------------------------------------------------*
           05  w-psg                      pic  9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio valore iniziale di un campo di *
      *        * tipo progressivo/anno, dopo normalizzazione           *
      *        *-------------------------------------------------------*
           05  w-psv                      pic  9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione moltiplicatore/divisore di  *
      *        * un campo di tipo progressivo/anno                     *
      *        *-------------------------------------------------------*
           05  w-pmd                      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione lunghezza del valore edita- *
      *        * to di un campo di tipo progressivo/anno               *
      *        *-------------------------------------------------------*
           05  w-prl                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Indice per unstring per accettazione campo di tipo    *
      *        * progressivo/anno                                      *
      *        *-------------------------------------------------------*
           05  w-pri                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Caratteri previsti in accettazioni numeriche          *
      *        *-------------------------------------------------------*
           05  w-cnp                      pic  x(17) value
                     "0123456789-,.EeLl"                              .
      *        *-------------------------------------------------------*
      *        * Comodo per parte intera di un numerico                *
      *        *-------------------------------------------------------*
           05  w-int                      pic  9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per parte decimale di un numerico              *
      *        *-------------------------------------------------------*
           05  w-dec                      pic  9(05)                  .
           05  w-dcr redefines w-dec.
               10  w-dcc occurs 05        pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero di interi gia' impostati in un numerico        *
      *        *-------------------------------------------------------*
           05  w-iim                      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero di decimali gia' impostati in un numerico      *
      *        *-------------------------------------------------------*
           05  w-dim                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero di virgole gia' impostate in un numerico       *
      *        *-------------------------------------------------------*
           05  w-vim                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero di segni algebrici gia' impostati in un nume-  *
      *        * rico                                                  *
      *        *-------------------------------------------------------*
           05  w-sim                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero di caratteri 'E' gia' impostati in un numerico *
      *        *-------------------------------------------------------*
           05  w-eim                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero di caratteri 'L' gia' impostati in un numerico *
      *        *-------------------------------------------------------*
           05  w-lim                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento campo di tipo 'V'                 *
      *        *-------------------------------------------------------*
           05  w-tcv                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Indice generico per utilizzi locali                   *
      *        *-------------------------------------------------------*
           05  w-inx                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di editing campo numerico con raggruppamento  *
      *        *  - Spaces : No                                        *
      *        *  - "G"   : Si                                         *
      *        *-------------------------------------------------------*
           05  w-rag                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di editing campo numerico con zeri in testa   *
      *        *  - Spaces : No                                        *
      *        *  - "9"   : Si                                         *
      *        *-------------------------------------------------------*
           05  w-zit                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di editing campo numerico con clausola 'blank *
      *        * when zero'                                            *
      *        *  - Spaces : No                                        *
      *        *  - "B"   : Si                                         *
      *        *-------------------------------------------------------*
           05  w-bwz                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di editing campo numerico con clausola 'deci- *
      *        * mali quanti effettivamente presenti'                  *
      *        *  - Spaces : No                                        *
      *        *  - "D"   : Si                                         *
      *        *-------------------------------------------------------*
           05  w-dqs                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di editing campo numerico con clausola 'nes-  *
      *        * sun editing dopo accettazione'                        *
      *        *  - Spaces : No                                        *
      *        *  - "N"   : Si                                         *
      *        *-------------------------------------------------------*
           05  w-noe                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di editing campo numerico con allineamento a  *
      *        * sinistra                                              *
      *        *  - Spaces : No                                        *
      *        *  - "<"   : Si                                         *
      *        *-------------------------------------------------------*
           05  w-asx                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di accettazione data con possibilita' di im-  *
      *        * postarla anche maggiore di quella di sistema          *
      *        *  - Spaces : No                                        *
      *        *  - ">"   : Si                                         *
      *        *-------------------------------------------------------*
           05  w-dma                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Segnale di presenza di una maschera di editing parti- *
      *        * colare                                                *
      *        *  - Spaces : No                                        *
      *        *  - "M"   : Si                                         *
      *        *-------------------------------------------------------*
           05  w-msk                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing campo numerico                       *
      *        *-------------------------------------------------------*
           05  w-ned.
               10  w-ned-chr    occurs 25 pic  x(01)                  .
           05  w-ne2 redefines w-ned.
               10  filler                 pic  x(01)                  .
               10  w-ned-rc9              pic  9.999.999.999.999,99999.
               10  w-ned-rs9 redefines
                   w-ned-rc9              pic  z.zzz.zzz.zzz.zz9,99999.
               10  filler                 pic  x(01)                  .
           05  w-ne3 redefines w-ned.
               10  filler                 pic  x(05)                  .
               10  w-ned-nc9              pic  9999999999999,99999    .
               10  w-ned-ns9 redefines
                   w-ned-nc9              pic  zzzzzzzzzzzz9,99999    .
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per ridefinizione parte intera di un  campo nu-  *
      *        * merico                                                *
      *        *-------------------------------------------------------*
           05  w-nem                      pic  9(13)                  .
           05  w-ney redefines w-nem.
               10  w-nex occurs 13        pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per ridefinizione immagine editata di un campo   *
      *        * di tipo progressivo/anno                              *
      *        *-------------------------------------------------------*
           05  w-ped.
               10  w-ped-pro              pic  z(10)                  .
               10  w-ped-bar              pic  x(01)                  .
               10  w-ped-ann              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio valore alfanumerico di max 132   *
      *        * caratteri                                             *
      *        *-------------------------------------------------------*
           05  w-svf.
               10  filler  occurs 132     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per salvataggio di un carattere alfanumerico     *
      *        *-------------------------------------------------------*
           05  w-sav                      pic  x(01)                  .
           05  w-new                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo per usi locali                    *
      *        *-------------------------------------------------------*
           05  w-ctr                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Pointer di comodo per usi locali                      *
      *        *-------------------------------------------------------*
           05  w-pnt                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio pointer precedente                        *
      *        *-------------------------------------------------------*
           05  w-svp                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione valore contenuto nella clip- *
      *        * board numerica                                        *
      *        *-------------------------------------------------------*
           05  w-cnn                      pic  9(13)v9(05)            .
           05  w-cnr redefines w-cnn.
               10  w-cnc occurs 18        pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio immagine linea 23                         *
      *        *-------------------------------------------------------*
           05  w-s23.
               10  filler  occurs 132     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio immagine linea 24                         *
      *        *-------------------------------------------------------*
           05  w-s24.
               10  filler  occurs 132     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Costante di 132 caratteri '-'                         *
      *        *-------------------------------------------------------*
           05  w-alm                      pic  x(132) value all "-"    .
      *        *-------------------------------------------------------*
      *        * Costante di 132 caratteri '_'                         *
      *        *-------------------------------------------------------*
           05  w-alu.
               10  w-unu                  pic  x(001) value     "_"    .
               10  filler                 pic  x(131) value all "_"    .
      *        *-------------------------------------------------------*
      *        * Work per Tipo funzionamento                           *
      *        *-------------------------------------------------------*
           05  w-tfu                      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per accettazione di :                            *
      *        *  - messaggio con risposta                             *
      *        *  - messaggio con x-risposta                           *
      *        *-------------------------------------------------------*
           05  w-mcr.
      *            *---------------------------------------------------*
      *            * Tipo operazione, MR o MX                          *
      *            *---------------------------------------------------*
               10  w-mcr-ope              pic  x(02)                  .
      *            *---------------------------------------------------*
      *            * Lista dei caratteri ammissibili                   *
      *            *---------------------------------------------------*
               10  w-mcr-lst              pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Lista delle function-keys ammissibili             *
      *            *---------------------------------------------------*
               10  w-mcr-fky.
                   15  filler occurs 160  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per funzione di Restore immagine video           *
      *        *-------------------------------------------------------*
           05  w-rst.
      *            *---------------------------------------------------*
      *            * Indice se restore inverso, da linea 24 a 1        *
      *            *---------------------------------------------------*
               10  w-rst-inv              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Indice per scansione 01..24                       *
      *            *---------------------------------------------------*
               10  w-rst-inx              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Field                                             *
      *            *---------------------------------------------------*
               10  w-rst-fld.
                   15  filler  occurs 133 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero linea                                      *
      *            *---------------------------------------------------*
               10  w-rst-lin              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero posizione                                  *
      *            *---------------------------------------------------*
               10  w-rst-pos              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Size                                              *
      *            *---------------------------------------------------*
               10  w-rst-siz              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Pointer                                           *
      *            *---------------------------------------------------*
               10  w-rst-pnt              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Save pointer                                      *
      *            *---------------------------------------------------*
               10  w-rst-svp              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatore per ciclo di attesa                     *
      *            *---------------------------------------------------*
               10  w-rst-wai              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Indicatore di caratteri in sospeso sulla tastiera per *
      *        * accept asincrona                                      *
      *        *-------------------------------------------------------*
           05  w-cip                      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Parametri per il lancio di un desk-accessory          *
      *        *-------------------------------------------------------*
           05  w-dac-prm.
      *            *---------------------------------------------------*
      *            * Per desk-accessory di tipo HELP - flag da utiliz- *
      *            * zare per abilitazione o disabilitazione           *
      *            *---------------------------------------------------*
               10  w-dac-prm-hlp-flg      pic  x(01) value spaces     .
      *            *---------------------------------------------------*
      *            * Per desk-accessory di tipo HELP                   *
      *            *---------------------------------------------------*
               10  w-dac-prm-hlp.
      *                *-----------------------------------------------*
      *                * Record number attuale del file di appoggio    *
      *                * 'xvi'                                         *
      *                *-----------------------------------------------*
                   15  w-dac-prm-hlp-rcn  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Modo di utilizzo attuale                      *
      *                *   - 0 :  80 colonne                           *
      *                *   - 1 : 132 colonne                           *
      *                *-----------------------------------------------*
                   15  w-dac-prm-hlp-zmo  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Ultimo modo di utilizzo effettivamente atti-  *
      *                * vato                                          *
      *                *   - 0 :  80 colonne                           *
      *                *   - 1 : 132 colonne                           *
      *                *-----------------------------------------------*
                   15  w-dac-prm-hlp-zum  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Area libera per usi futuri                    *
      *                *-----------------------------------------------*
                   15  filler             pic  x(33)                  .
      *            *---------------------------------------------------*
      *            * Per desk-accessory di tipo SHCP                   *
      *            *---------------------------------------------------*
               10  w-dac-prm-shc.
      *                *-----------------------------------------------*
      *                * Record number attuale del file di appoggio    *
      *                * 'xvi'                                         *
      *                *-----------------------------------------------*
                   15  w-dac-prm-shc-rcn  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Modo di utilizzo attuale                      *
      *                *   - 0 :  80 colonne                           *
      *                *   - 1 : 132 colonne                           *
      *                *-----------------------------------------------*
                   15  w-dac-prm-shc-zmo  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Ultimo modo di utilizzo effettivamente atti-  *
      *                * vato                                          *
      *                *   - 0 :  80 colonne                           *
      *                *   - 1 : 132 colonne                           *
      *                *-----------------------------------------------*
                   15  w-dac-prm-shc-zum  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Area libera per usi futuri                    *
      *                *-----------------------------------------------*
                   15  filler             pic  x(33)                  .

      *    *===========================================================*
      *    * Area per data di sistema                                  *
      *    *-----------------------------------------------------------*
       01  i.
      *        *-------------------------------------------------------*
      *        * Formato completo : s.aa.mm.gg                         *
      *        *-------------------------------------------------------*
           05  i-dat                      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione                                         *
      *        *-------------------------------------------------------*
           05  i-dt1 redefines i-dat.
      *            *---------------------------------------------------*
      *            * Secolo                                            *
      *            *---------------------------------------------------*
               10  i-scl                  pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Anno                                              *
      *            *---------------------------------------------------*
               10  i-ann                  pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Mese                                              *
      *            *---------------------------------------------------*
               10  i-mes                  pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Giorno                                            *
      *            *---------------------------------------------------*
               10  i-gio                  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione                                         *
      *        *-------------------------------------------------------*
           05  i-dt2 redefines i-dat.
      *            *---------------------------------------------------*
      *            * Secolo                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Formato a solo sei cifre : aa.mm.gg               *
      *            *---------------------------------------------------*
               10  i-dtc                  pic  9(06)                  .
           05  i-dts                      pic  9(07)                  .

      *    *===========================================================*
      *    * Work per operazioni di incremento o decremento delle date *
      *    * in giorni                                                 *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Work per routine det-dat-nrg-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-det-dat-nrg.
      *        *-------------------------------------------------------*
      *        * Data base per il calcolo                      [Input] *
      *        *-------------------------------------------------------*
           05  w-det-dat-nrg-dtb          pic  9(07)                  .
           05  w-det-dat-nrg-dtb-r redefines
               w-det-dat-nrg-dtb.
               10  w-det-dat-nrg-dba      pic  9(03)                  .
               10  w-det-dat-nrg-dbm      pic  9(02)                  .
               10  w-det-dat-nrg-dbg      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero giorni di incremento                   [Input] *
      *        *-------------------------------------------------------*
           05  w-det-dat-nrg-ngi          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Flag di primo passaggio                        [Work] *
      *        *-------------------------------------------------------*
           05  w-det-dat-nrg-fpp          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo giorni utilizzati                  [Work] *
      *        *-------------------------------------------------------*
           05  w-det-dat-nrg-pgu          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione numero giorni utilizzati    *
      *        *                                                [Work] *
      *        *-------------------------------------------------------*
           05  w-det-dat-nrg-ngu          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Data incrementata                            [Output] *
      *        *-------------------------------------------------------*
           05  w-det-dat-nrg-dti          pic  9(07)                  .
           05  w-det-dat-nrg-dti-r redefines
               w-det-dat-nrg-dti.
               10  w-det-dat-nrg-dia      pic  9(03)                  .
               10  w-det-dat-nrg-dim      pic  9(02)                  .
               10  w-det-dat-nrg-dig      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per routine det-nrg-dat-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-det-nrg-dat.
      *        *-------------------------------------------------------*
      *        * Data base per il calcolo                      [Input] *
      *        *-------------------------------------------------------*
           05  w-det-nrg-dat-dtb          pic  9(07)                  .
           05  w-det-nrg-dat-dtb-r redefines
               w-det-nrg-dat-dtb.
               10  w-det-nrg-dat-dba      pic  9(03)                  .
               10  w-det-nrg-dat-dbm      pic  9(02)                  .
               10  w-det-nrg-dat-dbg      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero giorni di decremento                   [Input] *
      *        *-------------------------------------------------------*
           05  w-det-nrg-dat-ngd          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Flag di primo passaggio                        [Work] *
      *        *-------------------------------------------------------*
           05  w-det-nrg-dat-fpp          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo giorni utilizzati                  [Work] *
      *        *-------------------------------------------------------*
           05  w-det-nrg-dat-pgu          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione numero giorni utilizzati    *
      *        *                                                [Work] *
      *        *-------------------------------------------------------*
           05  w-det-nrg-dat-ngu          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Data decrementata                            [Output] *
      *        *-------------------------------------------------------*
           05  w-det-nrg-dat-dtd          pic  9(07)                  .
           05  w-det-nrg-dat-dtd-r redefines
               w-det-nrg-dat-dtd.
               10  w-det-nrg-dat-dda      pic  9(03)                  .
               10  w-det-nrg-dat-ddm      pic  9(02)                  .
               10  w-det-nrg-dat-ddg      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione controllo            [Work] *
      *        *-------------------------------------------------------*
           05  w-det-nrg-dat-sts          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per system date + time                         *
      *        *-------------------------------------------------------*
           05  w-det-nrg-dat-sdt          pic  9(15)                  .
           05  w-det-nrg-dat-sdr redefines
               w-det-nrg-dat-sdt.
               10  w-det-nrg-dat-dat      pic  9(07)                  .
               10  w-det-nrg-dat-dtr redefines
                   w-det-nrg-dat-dat.
      *                *-----------------------------------------------*
      *                * Secolo e anno                                 *
      *                *-----------------------------------------------*
                   15  w-det-nrg-dat-saa  pic  9(03)                  .
                   15  w-det-nrg-dat-sar redefines
                       w-det-nrg-dat-saa.
      *                    *-------------------------------------------*
      *                    * Secolo                                    *
      *                    *-------------------------------------------*
                       20  w-det-nrg-dat-sec
                                          pic  9(01)                  .
      *                    *-------------------------------------------*
      *                    * Anno                                      *
      *                    *-------------------------------------------*
                       20  w-det-nrg-dat-ann
                                          pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Mese                                          *
      *                *-----------------------------------------------*
                   15  w-det-nrg-dat-mes  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Giorno                                        *
      *                *-----------------------------------------------*
                   15  w-det-nrg-dat-gio  pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Time                                              *
      *            *---------------------------------------------------*
               10  w-det-nrg-dat-tim      pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Tabella giorni del mese                               *
      *        *-------------------------------------------------------*
           05  w-tgm.
               10  w-tgn.
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 28         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
               10  w-tgo redefines
                   w-tgn.
                   15  w-tgp occurs 12    pic  9(02)                  .

      *    *===========================================================*
      *    * Area per determinazione tasto di terminazione usato       *
      *    *-----------------------------------------------------------*
       01  k.
      *        *-------------------------------------------------------*
      *        * Comodo per accettazione 1. 2. 3. 4. carattere dopo    *
      *        * ricognizione di 'Escape', per ovviare ad una incon-   *
      *        * sistenza tipica di Unix in trattamento 'Escape'       *
      *        *-------------------------------------------------------*
           05  k-acc                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Function-key determinata in accettazione              *
      *        *-------------------------------------------------------*
           05  k-key                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Tabella degli exception-code con riferimento alle re- *
      *        * lative function-keys riconosciute                     *
      *        *-------------------------------------------------------*
           05  k-tbl.
               10  filler                 pic  9(02) value 01         .
               10  filler                 pic  x(04) value     "[1] " .
               10  filler                 pic  9(02) value 02         .
               10  filler                 pic  x(04) value     "[2] " .
               10  filler                 pic  9(02) value 03         .
               10  filler                 pic  x(04) value     "[3] " .
               10  filler                 pic  9(02) value 04         .
               10  filler                 pic  x(04) value     "[4] " .
               10  filler                 pic  9(02) value 06         .
               10  filler                 pic  x(04) value     "RFSH" .
               10  filler                 pic  9(02) value 07         .
               10  filler                 pic  x(04) value     "COPY" .
               10  filler                 pic  9(02) value 08         .
               10  filler                 pic  x(04) value     "PAST" .
               10  filler                 pic  9(02) value 09         .
               10  filler                 pic  x(04) value     "EXPD" .
               10  filler                 pic  9(02) value 10         .
               10  filler                 pic  x(04) value     "APND" .
               10  filler                 pic  9(02) value 11         .
               10  filler                 pic  x(04) value     "ICHR" .
               10  filler                 pic  9(02) value 12         .
               10  filler                 pic  x(04) value     "DCHR" .
               10  filler                 pic  9(02) value 14         .
               10  filler                 pic  x(04) value     "BACK" .
               10  filler                 pic  9(02) value 15         .
               10  filler                 pic  x(04) value     "HELP" .
               10  filler                 pic  9(02) value 16         .
               10  filler                 pic  x(04) value     "DO  " .
               10  filler                 pic  9(02) value 17         .
               10  filler                 pic  x(04) value     "DELT" .
               10  filler                 pic  9(02) value 18         .
               10  filler                 pic  x(04) value     "PRNT" .
               10  filler                 pic  9(02) value 20         .
               10  filler                 pic  x(04) value     "EXIT" .
               10  filler                 pic  9(02) value 41         .
               10  filler                 pic  x(04) value     "FIND" .
               10  filler                 pic  9(02) value 42         .
               10  filler                 pic  x(04) value     "INSR" .
               10  filler                 pic  9(02) value 43         .
               10  filler                 pic  x(04) value     "REMV" .
               10  filler                 pic  9(02) value 44         .
               10  filler                 pic  x(04) value     "SLCT" .
               10  filler                 pic  9(02) value 45         .
               10  filler                 pic  x(04) value     "PRSC" .
               10  filler                 pic  9(02) value 46         .
               10  filler                 pic  x(04) value     "NXSC" .
               10  filler                 pic  9(02) value 51         .
               10  filler                 pic  x(04) value     "UP  " .
               10  filler                 pic  9(02) value 52         .
               10  filler                 pic  x(04) value     "DOWN" .
               10  filler                 pic  9(02) value 53         .
               10  filler                 pic  x(04) value     "RGHT" .
               10  filler                 pic  9(02) value 54         .
               10  filler                 pic  x(04) value     "LEFT" .
               10  filler                 pic  9(02) value 61         .
               10  filler                 pic  x(04) value     "RTRN" .
               10  filler                 pic  9(02) value 62         .
               10  filler                 pic  x(04) value     "TAB " .
               10  filler                 pic  9(02) value 65         .
               10  filler                 pic  x(04) value     "SHCP" .
           05  k-tbr redefines k-tbl.
               10  k-ele         occurs 30
                                 ascending key is k-val
                                 indexed       by k-inx               .
                   15  k-val              pic  9(02)                  .
                   15  k-fnc              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Tabella delle function-keys sempre ammesse            *
      *        *-------------------------------------------------------*
           05  k-tfs.
               10  filler                 pic  x(04) value     "    " .
               10  filler                 pic  x(04) value     "RTRN" .
               10  filler                 pic  x(04) value     "RGHT" .
               10  filler                 pic  x(04) value     "LEFT" .
               10  filler                 pic  x(04) value     "COPY" .
               10  filler                 pic  x(04) value     "PAST" .
               10  filler                 pic  x(04) value     "APND" .
               10  filler                 pic  x(04) value     "ICHR" .
               10  filler                 pic  x(04) value     "DCHR" .
               10  filler                 pic  x(04) value     "RFSH" .
           05  k-tft redefines k-tfs.
               10  k-tfu occurs 10 indexed  by k-tfx
                                          pic  x(04)                  .

      *    *===========================================================*
      *    * Work per prompt comuni ai programmi                       *
      *    *-----------------------------------------------------------*
       01  w-pmt.
      *        *-------------------------------------------------------*
      *        * Tabella di decodifica                                 *
      *        *-------------------------------------------------------*
           05  w-pmt-pgm.
      *            *---------------------------------------------------*
      *            * Tabella base                                      *
      *            *---------------------------------------------------*
               10  w-pmt-max              pic  9(03)       value 006  .
               10  w-pmt-lun              pic  9(02)       value 80   .
               10  w-pmt-ctr              pic  9(03)                  .
               10  w-pmt-tbl.
      *                *-----------------------------------------------*
      *                * 001 : '#SAV' Salvataggio registrazione        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(80) value
                     "Memorizzazione ? (S/N/E)                          
      -              "                              "                 .
      *                *-----------------------------------------------*
      *                * 002 : '#SAP' Conferma registrazione e stampa  *
      *                *-----------------------------------------------*
                   15  filler             pic  x(80) value
                     "Memorizzazione ? (S/N/E/P)                        
      -              "                              "                 .
      *                *-----------------------------------------------*
      *                * 003 : '#DEL' Cancellazione record             *
      *                *-----------------------------------------------*
                   15  filler             pic  x(80) value
                     "Cancellazione ? (S/N)                             
      -              "                              "                 .
      *                *-----------------------------------------------*
      *                * 004 : '#EXI' Uscita anche se modifiche        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(80) value
                     "Si rinuncia alle modifiche ? (S/N)                
      -              "                              "                 .
      *                *-----------------------------------------------*
      *                * 005 : '#VIS' Uscita da visualizzazione        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(80) value
                     "Uscita da visualizzazione ? (E)                   
      -              "                              "                 .
      *                *-----------------------------------------------*
      *                * 006 : '#CNF' Conferma impostazioni            *
      *                *-----------------------------------------------*
                   15  filler             pic  x(80) value
                     "Conferma ? (S/N/E)                                
      -              "                              "                 .
               10  w-pmt-tbr redefines
                   w-pmt-tbl.
                   15  w-pmt-ele occurs 006.
                       20  w-pmt-pmt      pic  x(80)                  .

      *    *===========================================================*
      *    * Per determinazione del giorno della settimana - 'day of   *
      *    * week'                                                     *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wgdslit0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

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

      ******************************************************************
       Procedure Division                 using z
                                                b
                                                v
                                                f-xvi                 .
      ******************************************************************

      *================================================================*
      *    Main                                                        *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Accept asincrona           *
      *              *-------------------------------------------------*
           if        v-ope                =    "AA"
                     perform aas-000      thru aas-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Display                    *
      *              *-------------------------------------------------*
           else if   v-ope                =    "DS"
                     perform dsp-000      thru dsp-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Note 1+2                   *
      *              *-------------------------------------------------*
           else if   v-ope                =    "NT"
                     perform not-000      thru not-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Nota 1                     *
      *              *-------------------------------------------------*
           else if   v-ope                =    "N1"
                     perform no1-000      thru no1-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Nota 2                     *
      *              *-------------------------------------------------*
           else if   v-ope                =    "N2"
                     perform no2-000      thru no2-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Edit                       *
      *              *-------------------------------------------------*
           else if   v-ope                =    "ED"
                     perform edt-000      thru edt-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Accept                     *
      *              *-------------------------------------------------*
           else if   v-ope                =    "AC"
                     perform acc-000      thru acc-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Erase                      *
      *              *-------------------------------------------------*
           else if   v-ope                =    "ER"
                     perform era-000      thru era-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Erase Lines                *
      *              *-------------------------------------------------*
           else if   v-ope                =    "EL"
                     perform erl-000      thru erl-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Set On                     *
      *              *-------------------------------------------------*
           else if   v-ope                =    "ON"
                     perform son-000      thru son-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Box grafico                *
      *              *-------------------------------------------------*
           else if   v-ope                =    "BG"
                     perform bgr-000      thru bgr-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Box                        *
      *              *-------------------------------------------------*
           else if   v-ope                =    "BX"
                     perform box-000      thru box-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Messaggio Errore           *
      *              *-------------------------------------------------*
           else if   v-ope                =    "ME"
                     perform mer-000      thru mer-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Messaggio Con Risposta     *
      *              *-------------------------------------------------*
           else if   v-ope                =    "MR"
                     perform mcr-000      thru mcr-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Messaggio Con X-Risposta   *
      *              *-------------------------------------------------*
           else if   v-ope                =    "MX"
                     perform mcr-000      thru mcr-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Refresh automatico         *
      *              *-------------------------------------------------*
           else if   v-ope                =    "RF"
                     perform rau-000      thru rau-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Convert                    *
      *              *-------------------------------------------------*
           else if   v-ope                =    "CV"
                     perform cvt-000      thru cvt-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Image Write                *
      *              *-------------------------------------------------*
           else if   v-ope                =    "IW"
                     perform imw-000      thru imw-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Image Read                 *
      *              *-------------------------------------------------*
           else if   v-ope                =    "IR"
                     perform imr-000      thru imr-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Save                       *
      *              *-------------------------------------------------*
           else if   v-ope                =    "SV"
                     perform sav-000      thru sav-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Restore                    *
      *              *-------------------------------------------------*
           else if   v-ope                =    "RS"
                     perform rst-000      thru rst-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Modo utilizzo 80           *
      *              *-------------------------------------------------*
           else if   v-ope                =    "M0"
                     perform mu0-000      thru mu0-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Modo utilizzo 132          *
      *              *-------------------------------------------------*
           else if   v-ope                =    "M1"
                     perform mu1-000      thru mu1-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Modo utilizzo ?            *
      *              *-------------------------------------------------*
           else if   v-ope                =    "M?"
                     perform muq-000      thru muq-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Disabilitazione desk-ac-   *
      *              *                      cessory                    *
      *              *-------------------------------------------------*
           else if   v-ope                =    "D-"
                     perform dda-000      thru dda-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Abilitazione desk-acces-   *
      *              *                      sory                       *
      *              *-------------------------------------------------*
           else if   v-ope                =    "D+"
                     perform eda-000      thru eda-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Tipo funzionamento         *
      *              *-------------------------------------------------*
           else if   v-ope                =    "TF"
                     perform tfu-000      thru tfu-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Open                       *
      *              *-------------------------------------------------*
           else if   v-ope                =    "OP"
                     perform opn-000      thru opn-999
      *              *-------------------------------------------------*
      *              * Se tipo operazione = Close                      *
      *              *-------------------------------------------------*
           else if   v-ope                =    "CL"
                     perform cls-000      thru cls-999                .
       main-999.
           exit      program                                          .
                     
      *================================================================*
      *    Routines                                                    *
      *================================================================*

      *================================================================*
      *    Open                                                        *
      *----------------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Richiesta del livello di esecuzione relativo ai *
      *              * desk-accessories e sua memorizzazione, insieme  *
      *              * al tipo di desk-accessory                       *
      *              *-------------------------------------------------*
           move      "LD"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-sts                to   z-lid                  .
           move      o-pat (1 : 4)        to   z-tda                  .
      *              *-------------------------------------------------*
      *              * Buffer immagine video a spaces                  *
      *              *-------------------------------------------------*
           move      spaces               to   b                      .
      *              *-------------------------------------------------*
      *              * Open file di appoggio per salvataggi su disco   *
      *              *                                                 *
      *              * Nota : Il file pathname e' quello passato dal   *
      *              *        modulo di segreteria, gia' eventualmen-  *
      *              *        te depurato dell'eccesso di prefisso     *
      *              *        per esecuzione di desk-accessory, per    *
      *              *        far si' che il file di appoggio sia      *
      *              *        condiviso da tutti i livelli di profon-  *
      *              *        dita' di esecuzione desk-accessory       *
      *              *-------------------------------------------------*
           open      i-o    xvi                                       .
       opn-200.
      *              *-------------------------------------------------*
      *              * Se si e' in esecuzione di un desk-accessory     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        z-lid                <    "01" or
                     z-lid                >    "99"
                     go to opn-999.
      *                  *---------------------------------------------*
      *                  * Lettura della variabile di environment      *
      *                  * V_DKB_PRMS passata al lancio dell'ese-      *
      *                  * cuzione di Tangram come desk-accessory      *
      *                  *---------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "PRMS"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione del valore della variabile   *
      *                  * di environment, che ridefinita indica :     *
      *                  *  - Il record number del file d'appoggio     *
      *                  *    per il video del Tangram chiamante al    *
      *                  *    momento della richiesta del desk-acces-  *
      *                  *    sory                                     *
      *                  *  - Il modo di utilizzo attuale, 80 o 132    *
      *                  *    colonne, del Tangram chiamante al mo-    *
      *                  *    mento della richiesta del desk-accessory *
      *                  *  - L'ultimo modo di utilizzo effettivamente *
      *                  *    attivato, 80 o 132 colonne, del Tangram  *
      *                  *    chiamante al momento della richiesta del *
      *                  *    desk-accessory                           *
      *                  *  - Un'area libera per utilizzi futuri       *
      *                  *---------------------------------------------*
           move      o-pat                to   w-dac-prm-hlp          .
      *                  *---------------------------------------------*
      *                  * Si pone il record-number del file di ap-    *
      *                  * poggio pari a quello del Tangram chiamante, *
      *                  * per condividere il file di appoggio, ma     *
      *                  * senza sovrapposizioni di Save/Restore       *
      *                  *---------------------------------------------*
           move      w-dac-prm-hlp-rcn    to   z-rcn                  .
      *                  *---------------------------------------------*
      *                  * Si pone il modo di utilizzo attuale in fun- *
      *                  * zione del modo di utilizzo attuale passato  *
      *                  * dal Tangram chiamante                       *
      *                  *---------------------------------------------*
           if        w-dac-prm-hlp-zmo    =    1
                     move  1              to   z-mod
                     move  132            to   z-mmx
           else      move  0              to   z-mod
                     move  080            to   z-mmx                  .
      *                  *---------------------------------------------*
      *                  * Si pone l'ultimo modo di utilizzo effetti-  *
      *                  * vamente attivato in funzione dell'ultimo    *
      *                  * modo di utilizzo effettivamente attivato    *
      *                  * passato dal Tangram chiiamante              *
      *                  *---------------------------------------------*
           move      w-dac-prm-hlp-zum    to   z-umu                  .
      *                  *---------------------------------------------*
      *                  * Lettura dell'immagine video salvata nel fi- *
      *                  * le di appoggio                              *
      *                  *---------------------------------------------*
           move      z-rcn                to   f-xvi-key              .
           perform   imr-000              thru imr-999                .
           move      f-xvi-dat            to   b                      .
       opn-999.
           exit.

      *================================================================*
      *    Close                                                       *
      *----------------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si e' in esecuzione di  *
      *              * un desk-accessory oppure no                     *
      *              *-------------------------------------------------*
           if        z-lid                not  < "01" and
                     z-lid                not  > "99"
                     go to cls-200.
       cls-100.
      *              *-------------------------------------------------*
      *              * Se non si e' in esecuzione di un desk-accessory *
      *              * ovvero si e' in una esecuzione principale       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione del modo di utilizzo, solo se    *
      *                  * necessario                                  *
      *                  *---------------------------------------------*
           perform   xmx-000              thru xmx-999                .
      *                  *---------------------------------------------*
      *                  * Erase screen                                *
      *                  *---------------------------------------------*
           display   omitted              erase                       .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     cls-900.
       cls-200.
      *              *-------------------------------------------------*
      *              * Se si e' in esecuzione di un desk-accessory     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Erase screen                                *
      *                  *---------------------------------------------*
      *    display   omitted              erase                       .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     cls-900.
       cls-900.
      *              *-------------------------------------------------*
      *              * Close file di appoggio per salvataggi su disco  *
      *              *-------------------------------------------------*
           close     xvi                                              .
       cls-999.
           exit.

      *================================================================*
      *    Save                                                        *
      *----------------------------------------------------------------*
       sav-000.
      *              *-------------------------------------------------*
      *              * Incremento max record number file appoggio      *
      *              *-------------------------------------------------*
           add       1                    to   z-rcn                  .
      *              *-------------------------------------------------*
      *              * Preparazione chiave record                      *
      *              *-------------------------------------------------*
           move      z-rcn                to   f-xvi-key              .
      *              *-------------------------------------------------*
      *              * Spostamento da buffer ad area record            *
      *              *-------------------------------------------------*
           move      b                    to   f-xvi-dat              .
      *              *-------------------------------------------------*
      *              * Completamento record con preparazione del modo  *
      *              * di utilizzo attuale, 80 o 132 colonne           *
      *              *-------------------------------------------------*
           move      z-mod                to   f-xvi-mod              .
      *              *-------------------------------------------------*
      *              * Image Write                                     *
      *              *-------------------------------------------------*
           perform   imw-000              thru imw-999                .
       sav-999.
           exit.

      *================================================================*
      *    Restore diretto, da linea 01 a linea 24                     *
      *----------------------------------------------------------------*
       rst-000.
      *              *-------------------------------------------------*
      *              * Segnale di restore diretto                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-rst-inv              .
      *              *-------------------------------------------------*
      *              * Restore effettivo                               *
      *              *-------------------------------------------------*
           perform   rst-exe-000          thru rst-exe-999            .
       rst-999.
           exit.

      *================================================================*
      *    Restore inverso, da linea 24 a linea 01                     *
      *----------------------------------------------------------------*
       rsi-000.
      *              *-------------------------------------------------*
      *              * Segnale di restore inverso                      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-rst-inv              .
      *              *-------------------------------------------------*
      *              * Restore effettivo                               *
      *              *-------------------------------------------------*
           perform   rst-exe-000          thru rst-exe-999            .
       rsi-999.
           exit.

      *================================================================*
      *    Restore effettivo                                           *
      *----------------------------------------------------------------*
       rst-exe-000.
      *              *-------------------------------------------------*
      *              * Preparazione chiave record                      *
      *              *-------------------------------------------------*
           move      z-rcn                to   f-xvi-key              .
      *              *-------------------------------------------------*
      *              * Decremento max record number file appoggio      *
      *              *-------------------------------------------------*
           subtract  1                    from z-rcn                  .
      *              *-------------------------------------------------*
      *              * Image Read                                      *
      *              *-------------------------------------------------*
           perform   imr-000              thru imr-999                .
      *              *-------------------------------------------------*
      *              * Ripristino, se necessario, del modo di utilizzo *
      *              *-------------------------------------------------*
           if        f-xvi-mod            =    1
                     perform mu1-000      thru mu1-999
           else      perform mu0-000      thru mu0-999                .
       rst-exe-400.
      *              *-------------------------------------------------*
      *              * Rivisualizzazione selettiva con memorizzazione  *
      *              *-------------------------------------------------*
       rst-exe-410.
      *                  *---------------------------------------------*
      *                  * Indice 01..24, o 24..01 : a zero o a 25     *
      *                  *---------------------------------------------*
           if        w-rst-inv            =    spaces
                     move  zero           to   w-rst-inx
           else      move  25             to   w-rst-inx              .
       rst-exe-420.
      *                  *---------------------------------------------*
      *                  * Incremento/Decremento indice 01..24         *
      *                  *---------------------------------------------*
           if        w-rst-inv            =    spaces
                     add      1           to   w-rst-inx
           else      subtract 1           from w-rst-inx              .
      *                  *---------------------------------------------*
      *                  * Se oltre 24, o a zero : uscita              *
      *                  *---------------------------------------------*
           if        w-rst-inv            =    spaces
                     if    w-rst-inx      >    24
                           go to rst-exe-999
                     else  go to rst-exe-430
           else      if    w-rst-inx      =    zero
                           go to rst-exe-999
                     else  go to rst-exe-430.
       rst-exe-430.
      *                  *---------------------------------------------*
      *                  * Se immagine sullo schermo gia' pari all'im- *
      *                  * magine da rivisualizzare : nessuna azione   *
      *                  *---------------------------------------------*
           if        f-xvi-lin(w-rst-inx) =    b-lin(w-rst-inx)
                     go to rst-exe-420.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri e display linea      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore campo da visualizzare            *
      *                      *-----------------------------------------*
           move      f-xvi-lin(w-rst-inx) to   w-rst-fld              .
      *                      *-----------------------------------------*
      *                      * Linea                                   *
      *                      *-----------------------------------------*
           move      w-rst-inx            to   w-rst-lin              .
      *                      *-----------------------------------------*
      *                      * Posizione                               *
      *                      *-----------------------------------------*
           move      01                   to   w-rst-pos              .
      *                      *-----------------------------------------*
      *                      * Size                                    *
      *                      *-----------------------------------------*
           move      z-mmx                to   w-rst-siz              .
       rst-exe-450.
      *                      *-----------------------------------------*
      *                      * Esecuzione display, analogo a quanto    *
      *                      * avviene in 'vis-mef-000/999'            *
      *                      *-----------------------------------------*
       rst-exe-460.
      *                          *-------------------------------------*
      *                          * Se avviene un display effettivo,    *
      *                          * preventivamente si esegue il modo   *
      *                          * di utilizzo, solo se necessario     *
      *                          *-------------------------------------*
           if        z-ooo                <    zero
                     move  high-value     to   z-ool(w-rst-lin)
                     go to rst-exe-470.
           perform   xmx-000              thru xmx-999                .
           display   w-rst-fld            line w-rst-lin
                                      position w-rst-pos
                                          size w-rst-siz              .
       rst-exe-470.
      *                          *-------------------------------------*
      *                          * Memorizzazione                      *
      *                          *-------------------------------------*
           add       1
                     w-rst-siz          giving w-rst-pnt              .
           move      w-rst-pnt            to   w-rst-svp              .
           move      high-value           to   w-chr(w-rst-pnt)       .
           move      w-rst-pos            to   w-rst-pnt              .
           string    w-rst-fld  delimited by   high-value
                                          into b-lin(w-rst-lin)
                                  with pointer w-rst-pnt              .
           move      spaces               to   w-chr(w-rst-svp)       .
       rst-exe-480.
      *                  *---------------------------------------------*
      *                  * Riciclo a linea successiva                  *
      *                  *---------------------------------------------*
           go to     rst-exe-420.
       rst-exe-999.
           exit.

      *================================================================*
      *    Modo di utilizzo a 80 colonne                               *
      *----------------------------------------------------------------*
       mu0-000.
      *              *-------------------------------------------------*
      *              * Se il modo di utilizzo richiesto corrisponde a  *
      *              * quello gia' attualmente in vigore: uscita sen-  *
      *              * za alcuna azione                                *
      *              *-------------------------------------------------*
           if        z-mod                =    0
                     go to mu0-999.
      *              *-------------------------------------------------*
      *              * Set del modo di utilizzo attuale ad indicare il *
      *              * modo di utilizzo a 80 colonne                   *
      *              *-------------------------------------------------*
           move      0                    to   z-mod                  .
      *              *-------------------------------------------------*
      *              * Set del max numero di colonne : 80              *
      *              *-------------------------------------------------*
           move      080                  to   z-mmx                  .
      *              *-------------------------------------------------*
      *              * Set del tipo di operazione in uscita ad indica- *
      *              * re il modo di utilizzo precedente a 132 colonne *
      *              *-------------------------------------------------*
           move      "M1"                 to   v-ope                  .
       mu0-999.
           exit.

      *================================================================*
      *    Modo di utilizzo a 132 colonne                              *
      *----------------------------------------------------------------*
       mu1-000.
      *              *-------------------------------------------------*
      *              * Se il modo di utilizzo richiesto corrisponde a  *
      *              * quello gia' attualmente in vigore: uscita sen-  *
      *              * za alcuna azione                                *
      *              *-------------------------------------------------*
           if        z-mod                =    1
                     go to mu1-999.
      *              *-------------------------------------------------*
      *              * Set del modo di utilizzo attuale ad indicare il *
      *              * modo di utilizzo a 132 colonne                  *
      *              *-------------------------------------------------*
           move      1                    to   z-mod                  .
      *              *-------------------------------------------------*
      *              * Set del max numero di colonne : 132             *
      *              *-------------------------------------------------*
           move      132                  to   z-mmx                  .
      *              *-------------------------------------------------*
      *              * Set del tipo di operazione in uscita ad indica- *
      *              * re il modo di utilizzo precedente a 80 colonne  *
      *              *-------------------------------------------------*
           move      "M0"                 to   v-ope                  .
       mu1-999.
           exit.

      *================================================================*
      *    Richiesta del modo di utilizzo attuale                      *
      *----------------------------------------------------------------*
       muq-000.
      *              *-------------------------------------------------*
      *              * Set del tipo di operazione in uscita a seconda  *
      *              * del modo di utilizzo attuale                    *
      *              *-------------------------------------------------*
           if        z-mod                =    1
                     move  "M1"           to   v-ope
           else      move  "M0"           to   v-ope                  .
       muq-999.
           exit.

      *================================================================*
      *    Disabilitazione desk-accessory                              *
      *----------------------------------------------------------------*
       dda-000.
      *              *-------------------------------------------------*
      *              * Disabilitazione desk-accessory                  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-dac-prm-hlp-flg      .
       dda-999.
           exit.

      *================================================================*
      *    Abilitazione desk-accessory                                 *
      *----------------------------------------------------------------*
       eda-000.
      *              *-------------------------------------------------*
      *              * Abilitazione desk-accessory                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-dac-prm-hlp-flg      .
       eda-999.
           exit.

      *================================================================*
      *    Tipo funzionamento                                          *
      *----------------------------------------------------------------*
       tfu-000.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-tfu                  .
      *
           if        v-tfu                =    "I"
                     move  "    inserimento"
                                          to   w-tfu
           else if   v-tfu                =    "M"
                     move  "       modifica"
                                          to   w-tfu
           else if   v-tfu                =    "V"
                     move  "visualizzazione"
                                          to   w-tfu
           else      move  spaces         to   w-tfu                  .
      *              *-------------------------------------------------*
      *              * Tipo funzionamento in presenza di modifica      *
      *              * effettiva                                       *
      *              *-------------------------------------------------*
           if        v-tfu                =    "M" and
                     v-tfm                not  = spaces
                     move  "   (!) MODIFICA"
                                          to   w-tfu                  .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo funzionamento a riga 2     *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      02                   to   w-lin                  .
           move      66                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           move      w-tfu                to   w-fld                  .
           perform   vis-mef-000          thru vis-mef-999            .
       tfu-999.
           exit.

      *================================================================*
      *    Esecuzione del modo di utilizzo se necessario               *
      *----------------------------------------------------------------*
       xmx-000.
      *              *-------------------------------------------------*
      *              * Se il modo di utilizzo attuale e' pari all'ul-  *
      *              * timo modo di utilizzo effettivamente attivato : *
      *              * uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           if        z-mod                =    z-umu
                     go to xmx-999.
      *              *-------------------------------------------------*
      *              * Altrimenti si esegue il modo di utilizzo attua- *
      *              * le                                              *
      *              *-------------------------------------------------*
           if        z-mod                =    1
                     perform xm1-000      thru xm1-999
           else      perform xm0-000      thru xm0-999                .
      *              *-------------------------------------------------*
      *              * E poi si pone l'ultimo modo di utilizzo effet-  *
      *              * tivamente attivato pari a quello attuale        *
      *              *-------------------------------------------------*
           move      z-mod                to   z-umu                  .
       xmx-999.
           exit.

      *================================================================*
      *    Esecuzione del modo di utilizzo a 80 colonne                *
      *----------------------------------------------------------------*
       xm0-000.
           display   screen size 80.
       xm0-999.
           exit.

      *================================================================*
      *    Esecuzione del modo di utilizzo a 132 colonne               *
      *----------------------------------------------------------------*
       xm1-000.
           display   screen size 132.
       xm1-999.
           exit.

      *================================================================*
      *    Image Write                                                 *
      *----------------------------------------------------------------*
       imw-000.
      *              *-------------------------------------------------*
      *              * Area record in area file                        *
      *              *-------------------------------------------------*
           move      f-xvi-rec            to   xvi-rec                .
       imw-200.
      *              *-------------------------------------------------*
      *              * Prima si tenta una riscrittura, in quanto e'    *
      *              * statisticamente piu' probabile che il record    *
      *              * esista gia' come frutto di una interrogazione   *
      *              * precedente                                      *
      *              *-------------------------------------------------*
           rewrite   xvi-rec invalid key
                             go to   imw-400.
           go to     imw-999.
       imw-400.
      *              *-------------------------------------------------*
      *              * Se la riscrittura non ha avuto buon esito, si-  *
      *              * gnifica che siamo alla prima memorizzazione di  *
      *              * questo record, ed in questo caso si esegue una  *
      *              * scrittura                                       *
      *              *-------------------------------------------------*
           write     xvi-rec invalid key
                             go to   imw-200.
       imw-999.
           exit.

      *================================================================*
      *    Image Read                                                  *
      *----------------------------------------------------------------*
       imr-000.
      *              *-------------------------------------------------*
      *              * Lettura record da disco                         *
      *              *-------------------------------------------------*
           read      xvi    with no lock
                            invalid key
                            go to   imr-200.
       imr-200.
      *              *-------------------------------------------------*
      *              * Area file in area record                        *
      *              *-------------------------------------------------*
           move      xvi-rec              to   f-xvi-rec              .
       imr-999.
           exit.

      *================================================================*
      *    Accept asincrona                                            *
      *----------------------------------------------------------------*
       aas-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametro 'o-mxf', per la linea    *
      *              * per l'accettazione asincrona, per 'mopsys'      *
      *              *-------------------------------------------------*
           move      v-lin                to   o-mxf                  .
      *              *-------------------------------------------------*
      *              * Preparazione parametro 'o-mxe', per la posizio- *
      *              * ne per l'accettazione asincrona, per 'mopsys'   *
      *              *-------------------------------------------------*
           move      v-pos                to   o-mxe                  .
      *              *-------------------------------------------------*
      *              * Preparazione parametro 'o-com', con i tasti fun-*
      *              * zione, per il richiamo del modulo 'mopsys'      *
      *              *-------------------------------------------------*
           move      spaces               to   o-com                  .
           move      zero                 to   w-inx                  .
           move      zero                 to   w-ctr                  .
       aas-010.
           add       1                    to   w-inx                  .
           if        w-inx                >    40
                     go to aas-100.
           if        v-pfk
                    (w-inx)               =    spaces
                     go to aas-010.
           add       1                    to   w-ctr                  .
           move      w-ctr                to   w-pnt                  .
           multiply  4                    by   w-pnt                  .
           subtract  3                    from w-pnt                  .
           move      v-pfk
                    (w-inx)               to   o-com (w-pnt : 4)      .
           if        w-ctr                <    10
                     go to aas-010.
       aas-100.
      *              *-------------------------------------------------*
      *              * Richiamo modulo 'mopsys' per esecuzione effet-  *
      *              * tiva della funzione                             *
      *              *-------------------------------------------------*
           move      "=K"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                        using  o                      .
      *              *-------------------------------------------------*
      *              * Preparazione del valore in uscita               *
      *              *-------------------------------------------------*
           move      o-com (1 : 4)        to   v-key                  .
      *              *-------------------------------------------------*
      *              * Normalizzazione f-keys possibili                *
      *              *-------------------------------------------------*
           move      spaces               to   v-ufk                  .
       aas-999.
           exit.

      *================================================================*
      * Display                                                        *
      *----------------------------------------------------------------*
       dsp-000.
      *              *-------------------------------------------------*
      *              * ___ VIRUS !!! ___                               *
      *              *-------------------------------------------------*
______*    move      all "#"              to   v-alf                  .
______*    move      all 9                to   v-num                  .
______*    move      all 9                to   v-dat                  .
       
      *              *-------------------------------------------------*
      *              * Se alfa                                         *
      *              *-------------------------------------------------*
           if        v-tip                =    "A" or
                     v-tip                =    "U" or
                     v-tip                =    "L" 
                     perform dsp-alf-000  thru dsp-alf-999
      *              *-------------------------------------------------*
      *              * Se text                                         *
      *              *-------------------------------------------------*
           else if   v-tip                =    "T"
                     perform dsp-txt-000  thru dsp-txt-999
      *              *-------------------------------------------------*
      *              * Se espanso                                      *
      *              *-------------------------------------------------*
           else if   v-tip                =    "E"
                     perform dsp-esp-000  thru dsp-esp-999
      *              *-------------------------------------------------*
      *              * Se check-box                                    *
      *              *-------------------------------------------------*
           else if   v-tip                =    "C"
                     perform dsp-ckb-000  thru dsp-ckb-999
      *              *-------------------------------------------------*
      *              * Se numerico                                     *
      *              *-------------------------------------------------*
           else if   v-tip                =    "N" or
                     v-tip                =    "V"
                     perform dsp-num-000  thru dsp-num-999
      *              *-------------------------------------------------*
      *              * Se progressivo/anno                             *
      *              *-------------------------------------------------*
           else if   v-tip                =    "P"
                     perform dsp-pga-000  thru dsp-pga-999
      *              *-------------------------------------------------*
      *              * Se data                                         *
      *              *-------------------------------------------------*
           else if   v-tip                =    "D"
                     perform dsp-dat-000  thru dsp-dat-999
      *              *-------------------------------------------------*
      *              * Se password                                     *
      *              *-------------------------------------------------*
           else if   v-tip                =    "W"
                     perform dsp-pwd-000  thru dsp-pwd-999            .
       dsp-999.
           exit.

      *================================================================*
      *    Edit                                                        *
      *----------------------------------------------------------------*
       edt-000.
      *              *-------------------------------------------------*
      *              * Se alfa                                         *
      *              *-------------------------------------------------*
           if        v-tip                =    "A" or
                     v-tip                =    "U" or
                     v-tip                =    "L" 
                     move  v-alf          to   v-edt
                     move  v-car          to   v-edl
      *              *-------------------------------------------------*
      *              * Se text                                         *
      *              *-------------------------------------------------*
           else if   v-tip                =    "T"
                     move  v-txt (1 : v-car)
                                          to   v-edt
                     move  v-car          to   v-edl
      *              *-------------------------------------------------*
      *              * Se espanso                                      *
      *              *-------------------------------------------------*
           else if   v-tip                =    "E"
                     perform edt-esp-000  thru edt-esp-999
      *              *-------------------------------------------------*
      *              * Se check-box                                    *
      *              *-------------------------------------------------*
           else if   v-tip                =    "C"
                     perform edt-ckb-000  thru edt-ckb-999
      *              *-------------------------------------------------*
      *              * Se numerico                                     *
      *              *-------------------------------------------------*
           else if   v-tip                =    "N" or
                     v-tip                =    "V"
                     perform edt-num-000  thru edt-num-999
      *              *-------------------------------------------------*
      *              * Se progressivo/anno                             *
      *              *-------------------------------------------------*
           else if   v-tip                =    "P"
                     perform edt-pga-000  thru edt-pga-999
      *              *-------------------------------------------------*
      *              * Se data                                         *
      *              *-------------------------------------------------*
           else if   v-tip                =    "D"
                     perform edt-dat-000  thru edt-dat-999            .
       edt-999.
           exit.

      *================================================================*
      *    Accept                                                      *
      *----------------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Set del carattere di underline : se Find ammes- *
      *              * so : "." , altrimenti "_"                       *
      *              *-------------------------------------------------*
           if        v-pfk (03)           =    "FIND"
                     move  all "."        to   w-alu
           else      move  all "_"        to   w-alu                  .
      *              *-------------------------------------------------*
      *              * Se alfa o alfa-uppercase o alfa-lowercase       *
      *              *-------------------------------------------------*
           if        v-tip                =    "A"      or
                     v-tip                =    "U"      or
                     v-tip                =    "L"
                     perform acc-alf-000  thru acc-alf-999
      *              *-------------------------------------------------*
      *              * Se text                                         *
      *              *-------------------------------------------------*
           else if   v-tip                =    "T"
                     perform acc-txt-000  thru acc-txt-999
      *              *-------------------------------------------------*
      *              * Se espanso                                      *
      *              *-------------------------------------------------*
           else if   v-tip                =    "E"
                     perform acc-esp-000  thru acc-esp-999
      *              *-------------------------------------------------*
      *              * Se check-box                                    *
      *              *-------------------------------------------------*
           else if   v-tip                =    "C"
                     perform acc-ckb-000  thru acc-ckb-999
      *              *-------------------------------------------------*
      *              * Se numerico                                     *
      *              *-------------------------------------------------*
           else if   v-tip                =    "N" or
                     v-tip                =    "V"
                     perform acc-num-000  thru acc-num-999
      *              *-------------------------------------------------*
      *              * Se progressivo/anno                             *
      *              *-------------------------------------------------*
           else if   v-tip                =    "P"
                     perform acc-pga-000  thru acc-pga-999
      *              *-------------------------------------------------*
      *              * Se data                                         *
      *              *-------------------------------------------------*
           else if   v-tip                =    "D"
                     perform acc-dat-000  thru acc-dat-999
      *              *-------------------------------------------------*
      *              * Se function-key                                 *
      *              *-------------------------------------------------*
           else if   v-tip                =    "K"
                     perform acc-fky-000  thru acc-fky-999
      *              *-------------------------------------------------*
      *              * Se word-key                                     *
      *              *-------------------------------------------------*
           else if   v-tip                =    "W"
                     perform acc-wky-000  thru acc-wky-999            .
       acc-999.
           exit.

      *================================================================*
      *    Erase                                                       *
      *----------------------------------------------------------------*
       era-000.
      *              *-------------------------------------------------*
      *              * Test se video attualmente in On oppure in Off   *
      *              *-------------------------------------------------*
           if        z-ooo                not  < zero
                     go to era-500.
      *                  *---------------------------------------------*
      *                  * Se video in Off                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Erase Line da 01  a 24                  *
      *                      *-----------------------------------------*
           move      01                   to   v-lin                  .
           move      24                   to   v-lto                  .
           perform   erl-000              thru erl-999                .
           go to     era-999.
       era-500.
      *                  *---------------------------------------------*
      *                  * Se video in On                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Abblencamento buffer video              *
      *                      *-----------------------------------------*
           move      spaces               to   b                      .
      *                      *-----------------------------------------*
      *                      * Esecuzione del modo di utilizzo, solo   *
      *                      * se necessario                           *
      *                      *-----------------------------------------*
           perform   xmx-000              thru xmx-999                .
      *                      *-----------------------------------------*
      *                      * Erase effettivo                         *
      *                      *-----------------------------------------*
           display   omitted              line 01 position 01
                                          erase                       .
       era-999.
           exit.

      *================================================================*
      *    Erase Lines                                                 *
      *----------------------------------------------------------------*
       erl-000.
      *              *-------------------------------------------------*
      *              * Puntatore su numero linea da abblencare         *
      *              *-------------------------------------------------*
           move      v-lin                to   w-ctr                  .
       erl-100.
      *              *-------------------------------------------------*
      *              * Test se linea gia' a blank                      *
      *              *-------------------------------------------------*
           if        b-lin (w-ctr)        =    spaces
                     go to erl-200.
      *              *-------------------------------------------------*
      *              * Abblencamento linea in buffer                   *
      *              *-------------------------------------------------*
           move      spaces               to   b-lin(w-ctr)           .
      *              *-------------------------------------------------*
      *              * Se video in on visualizzazione della linea ab-  *
      *              * blencata altrimenti suo contrassegno            *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *                                                 *
      *              * Se avviene un display effettivo, preventivamen- *
      *              * te si esegue il modo di utilizzo, solo se ne-   *
      *              * cessario                                        *
      *              *-------------------------------------------------*
           if        z-ooo                <    zero
                     move    high-values  to   z-ool(w-ctr)
           else      perform xmx-000      thru xmx-999
                     display spaces       line w-ctr
                                      position 01
                                          size z-mmx                  .
       erl-200.
      *              *-------------------------------------------------*
      *              * Avanzamento puntatore su prossima linea         *
      *              *-------------------------------------------------*
           if        w-ctr                <    v-lto
                     add     1            to   w-ctr
                     go to   erl-100.
       erl-999.
           exit.
           
      *================================================================*
      *    Set On                                                      *
      *----------------------------------------------------------------*
       son-000.
      *              *-------------------------------------------------*
      *              * Incremento del livello di 'on'                  *
      *              *-------------------------------------------------*
           add       1                    to   z-ooo                  .
      *              *-------------------------------------------------*
      *              * Se tuttora in 'off' oppure se si era gia' in    *
      *              * 'on' non si esegue nessuna azione               *
      *              *-------------------------------------------------*
           if        z-ooo                not  = zero
                     go to son-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione linee in sospeso di visualizza- *
      *              * zione                                           *
      *              *-------------------------------------------------*
       son-500.
      *                     *------------------------------------------*
      *                     * Ricerca prossima linea in sospeso        *
      *                     *------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   z-oob            tallying w-ctr
                     for    characters  before initial high-value     .
      *                     *------------------------------------------*
      *                     * Se nessuna linea in sospeso, oltre       *
      *                     *------------------------------------------*
           if        w-ctr                =    24
                     go to  son-800.
           add       1                    to   w-ctr                  .
      *                     *------------------------------------------*
      *                     * Marcatura linea non piu' in sospeso      *
      *                     *------------------------------------------*
           move      low-value            to   z-ool(w-ctr)           .
       son-600.
      *                      *-----------------------------------------*
      *                      * Esecuzione del modo di utilizzo, solo   *
      *                      * se necessario                           *
      *                      *-----------------------------------------*
           perform   xmx-000              thru xmx-999                .
      *                     *------------------------------------------*
      *                     * Visualizzazione fisica linea in sospeso  *
      *                     *                                          *
      *                     * Differenziata a seconda se modo a 80 co- *
      *                     * lonne o a 132 colonne                    *
      *                     *------------------------------------------*
           if        b-lin (w-ctr)        =    spaces
                     display  spaces      line w-ctr
                                      position 01
                                          size z-mmx
           else      display  b-lin (w-ctr)
                                          line w-ctr
                                      position 01
                                          size z-mmx                  .
      *                      *-----------------------------------------*
      *                      * A linea successiva                      *
      *                      *-----------------------------------------*
           go to     son-500.
       son-800.
      *              *-------------------------------------------------*
      *              * Eventuale Box in sospeso                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su switch                              *
      *                  *---------------------------------------------*
           if        z-bxs                =    0
                     go to son-999.
      *                  *---------------------------------------------*
      *                  * Visualizzazione box grafico in sospeso      *
      *                  *---------------------------------------------*
           move      w-box-sli            to   w-box-lin              .
           move      w-box-spo            to   w-box-pos              .
           move      w-box-slt            to   w-box-lto              .
           move      w-box-spt            to   w-box-pto              .
           perform   bxg-000              thru bxg-999                .
      *                  *---------------------------------------------*
      *                  * Disattivazione switch                       *
      *                  *---------------------------------------------*
           move      0                    to   z-bxs                  .
       son-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore di una o due righe            *
      *    *                                                           *
      *    * MEMO: si potrebbe implementare il tipo 'v-tip' per poter  *
      *    *       scegliere se l'accettazione del carattere e' libera *
      *    *       o deve corrispondere al carattere passato in        *
      *    *       'v-sgn'                                             *
      *    *-----------------------------------------------------------*
       bxe-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           perform   sav-000              thru sav-999                .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           subtract  1                    from z-ooo                  .
      *              *-------------------------------------------------*
      *              * Box normale                                     *
      *              *-------------------------------------------------*
           if        v-nt1                not  = spaces
                     move 11              to   v-lin
           else      move 12              to   v-lin                  .
      *
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           perform   box-000              thru box-999                .
       bxe-100.
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           if        v-nt1                =    spaces
                     go to bxe-200.
      *
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      v-nt1                to   v-alf                  .
           perform   dsp-000              thru dsp-999                .
       bxe-200.
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      v-nt2                to   v-alf                  .
           perform   dsp-000              thru dsp-999                .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           perform   dsp-000              thru dsp-999                .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           perform   son-000              thru son-999                .
      *              *-------------------------------------------------*
      *              * Box grafico eventuale                           *
      *              *-------------------------------------------------*
           if        v-nt1                not  = spaces
                     move 11              to   w-box-lin
           else      move 12              to   w-box-lin              .
      *
           move      04                   to   w-box-pos              .
           move      14                   to   w-box-lto              .
           move      77                   to   w-box-pto              .
           perform   bxg-000              thru bxg-999                .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           perform   acc-fky-000          thru acc-fky-999            .
      *              *-------------------------------------------------*
      *              * Si riporta v-not con v-nt1 e v-nt2 a spaces     *
      *              *-------------------------------------------------*
           move      spaces               to   v-not                  .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           perform   rst-000              thru rst-999                .
       bxe-999.
           exit.

      *    *===========================================================*
      *    * Box grafico                                               *
      *    *-----------------------------------------------------------*
       bgr-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione box grafico                     *
      *              *-------------------------------------------------*
           move      v-lin                to   w-box-lin              .
           move      v-pos                to   w-box-pos              .
           move      v-lto                to   w-box-lto              .
           move      v-pto                to   w-box-pto              .
           perform   bxg-000              thru bxg-999                .
       bgr-999.
           exit.

      *    *===========================================================*
      *    * Box grafico                                               *
      *    *-----------------------------------------------------------*
       bxg-000.
      *              *-------------------------------------------------*
      *              * Test se visualizzabile                          *
      *              *-------------------------------------------------*
           if        z-mdg                not  = 1
                     go to bxg-999.
      *              *-------------------------------------------------*
      *              * Calcolo dimensioni box grafico                  *
      *              *-------------------------------------------------*
           subtract  w-box-pos            from w-box-pto
                                        giving w-siz                  .
           add       1                    to   w-siz                  .
      *
           subtract  w-box-lin            from w-box-lto
                                        giving w-lin                  .
           add       1                    to   w-lin                  .
      *              *-------------------------------------------------*
      *              * Box grafico                                     *
      *              *-------------------------------------------------*
           display   box               at line w-box-lin
                                      position w-box-pos
                                          size w-siz
                                         lines w-lin                  .
       bxg-999.
           exit.

      *    *===========================================================*
      *    * Box                                                       *
      *    *-----------------------------------------------------------*
       box-000.
      *              *-------------------------------------------------*
      *              * Attivazione switch di Box in sospeso            *
      *              *-------------------------------------------------*
           move      1                    to   z-bxs                  .
      *              *-------------------------------------------------*
      *              * Salvataggio coordinate                          *
      *              *-------------------------------------------------*
           move      v-lin                to   w-box-sli              .
           move      v-pos                to   w-box-spo              .
           move      v-lto                to   w-box-slt              .
           move      v-pto                to   w-box-spt              .
      *              *-------------------------------------------------*
      *              * Preparazione line - position - size             *
      *              *-------------------------------------------------*
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           subtract  v-pos                from v-pto
                                        giving w-siz                  .
           add       1                    to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Linea superiore                                 *
      *              *-------------------------------------------------*
           move      w-alm                to   w-fld                  .
           move      "+"                  to   w-chr (1)              .
           move      "+"                  to   w-chr (w-siz)          .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Linee intermedie                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fld                  .
           move      "|"                  to   w-chr (1)              .
           move      "|"                  to   w-chr (w-siz)          .
           subtract  v-lin                from v-lto
                                        giving w-ctr                  .
           subtract  1                    from w-ctr                  .
       box-500.
           add       1                    to   w-lin                  .
           if        w-ctr                >    zero
                     subtract 1           from w-ctr
                     perform  vis-mef-000 thru vis-mef-999
                     go to    box-500.
      *              *-------------------------------------------------*
      *              * Linea inferiore                                 *
      *              *-------------------------------------------------*
           move      w-alm                to   w-fld                  .
           move      "+"                  to   w-chr (1)              .
           move      "+"                  to   w-chr (w-siz)          .
           perform   vis-mef-000          thru vis-mef-999            .
       box-999.
           exit.

      *    *===========================================================*
      *    * Messaggio Errore a riga 24                                *
      *    *-----------------------------------------------------------*
       mer-000.
      *              *-------------------------------------------------*
      *              * Sostituito dalla nuova funzione di messaggio    *
      *              * con box                                         *
      *              *-------------------------------------------------*
           perform   bxe-000              thru bxe-999                .
           go to     mer-999.
      *
       mer-100.
      *              *-------------------------------------------------*
      *              * Salvataggio linee 23 e 24 attuali               *
      *              *-------------------------------------------------*
           move      b-lin (23)           to   w-s23                  .
           move      b-lin (24)           to   w-s24                  .
      *              *-------------------------------------------------*
      *              * Visualizzazione nuova linea 23                  *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      v-nt1                to   w-fld                  .
           move      23                   to   w-lin                  .
           move      01                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Nota 2 in campo di comodo                       *
      *              *-------------------------------------------------*
           move      v-nt2                to   w-fld                  .
      *              *-------------------------------------------------*
      *              * Integrazione della linea 24 con i caratteri di  *
      *              * parentesi per accettazione presa visione        *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      z-mmx                to   z-mmy                  .
           subtract  4                    from z-mmy                  .
           move      " [ ] "              to   w-fld (z-mmy : 5)      .
      *              *-------------------------------------------------*
      *              * Visualizzazione nuova linea 24                  *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      24                   to   w-lin                  .
           move      01                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione a linea *
      *              * 24, a tutta destra                              *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      z-mmx                to   w-pos                  .
           subtract  2                    from w-pos                  .
           perform   acc-key-000          thru acc-key-999            .
      *              *-------------------------------------------------*
      *              * Ripristino linee 23 e 24 salvate in precedenza  *
      *              *                                                 *
      *              * Differenziato a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      w-s23                to   w-fld                  .
           move      23                   to   w-lin                  .
           move      01                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
           move      w-s24                to   w-fld                  .
           move      24                   to   w-lin                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Si riporta v-not con v-nt1 e v-nt2 a spaces     *
      *              *-------------------------------------------------*
           move      spaces               to   v-not                  .
       mer-999.
           exit.

      *================================================================*
      * Messaggio con risposta e Messaggio con x-risposta              *
      *----------------------------------------------------------------*
       mcr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni se con x-risposta               *
      *              *-------------------------------------------------*
           move      v-ope                to   w-mcr-ope              .
           move      v-msk                to   w-mcr-lst              .
           if        w-mcr-ope            =    "MX"
                     move  1              to   v-car                  .
       mcr-010.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari se con x-risposta        *
      *              *-------------------------------------------------*
           if        w-mcr-ope            not  = "MX"
                     go to mcr-050.
       mcr-020.
      *              *-------------------------------------------------*
      *              * Estrazione eventuale del prompt da tabella      *
      *              *-------------------------------------------------*
           if        v-not (01 : 01)      not  = "#"
                     go to mcr-050.
      *              *-------------------------------------------------*
      *              * Prompt da tabella                               *
      *              *-------------------------------------------------*
           move      001                  to   w-pmt-ctr              .
           if        v-not (01 : 04)      =    "#SAV"
                     move  001            to   w-pmt-ctr
           else if   v-not (01 : 04)      =    "#SAP"
                     move  002            to   w-pmt-ctr
           else if   v-not (01 : 04)      =    "#DEL"
                     move  003            to   w-pmt-ctr
           else if   v-not (01 : 04)      =    "#EXI"
                     move  004            to   w-pmt-ctr
           else if   v-not (01 : 04)      =    "#VIS"
                     move  005            to   w-pmt-ctr
           else if   v-not (01 : 04)      =    "#CNF"
                     move  006            to   w-pmt-ctr              .
      *
           move      w-pmt-ele
                    (w-pmt-ctr)           to   v-not                  .
       mcr-050.
      *              *-------------------------------------------------*
      *              * Salvataggio linee 23 e 24 attuali               *
      *              *-------------------------------------------------*
           move      b-lin (23)           to   w-s23                  .
           move      b-lin (24)           to   w-s24                  .
      *              *-------------------------------------------------*
      *              * Visualizzazione nuova linea 23                  *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      v-nt1                to   w-fld                  .
           move      23                   to   w-lin                  .
           move      01                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Visualizzazione nuova linea 24                  *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      v-nt2                to   w-fld                  .
      *
           if        v-tip                =    "N" or
                     v-tip                =    "V"
                     move    v-num        to   w-num
                     perform car-edn-000  thru car-edn-999
           else if   v-tip                =    "D"
                     move    8            to   v-edl
           else      move    v-car        to   v-edl                  .
      *
           move      v-nt2                to   w-fld                  .
           subtract  v-edl , 2            from z-mmx
                                        giving w-ccr                  .
       mcr-100.
           if        w-ccr                not  > z-mmx
                     move    spaces       to   w-chr (w-ccr)
                     add     1            to   w-ccr
                     go to   mcr-100.
      *
           subtract  v-edl , 3            from z-mmx
                                        giving v-pos                  .
      *
           if        v-nt1                =    spaces and
                     v-nt2                =    spaces
                     add   1              to   v-pos
                     go to mcr-300.
       mcr-200.
           if        w-chr(v-pos)         =    spaces
                     if      v-pos        >    1
                             subtract 1   from v-pos
                             go to mcr-200
                     else    go to mcr-300.
      *
           add       2                    to   v-pos                  .
       mcr-300.
           move      "["                  to   w-chr (v-pos)          .
           add       v-edl , 1            to   v-pos                  .
           move      "]"                  to   w-chr (v-pos)          .
           move      24                   to   w-lin                  .
           move      01                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Preparazione parametri residui per accettazione *
      *              *-------------------------------------------------*
           subtract  v-edl                from v-pos                  .
           move      24                   to   v-lin                  .
       mcr-500.
      *              *-------------------------------------------------*
      *              * Salvataggio function keys                       *
      *              *-------------------------------------------------*
           move      v-ufk                to   w-mcr-fky              .
       mcr-550.
      *              *-------------------------------------------------*
      *              * Ripristino function keys                        *
      *              *-------------------------------------------------*
           move      w-mcr-fky            to   v-ufk                  .
      *              *-------------------------------------------------*
      *              * Accettazione del campo di risposta              *
      *              *-------------------------------------------------*
           perform   acc-000              thru acc-999                .
      *              *-------------------------------------------------*
      *              * Controlli se con x-risposta                     *
      *              *-------------------------------------------------*
           if        w-mcr-ope            not  = "MX"
                     go to mcr-900.
           if        v-key                not  = spaces
                     go to mcr-900.
           move      v-alf                to   w-sav                  .
           if        w-mcr-lst            =    spaces
                     go to mcr-900.
           if        w-sav                =    spaces
                     go to mcr-550.
           move      zero                 to   w-ctr                  .
           inspect   w-mcr-lst        tallying w-ctr
                     for                  all  w-sav                  .
           if        w-ctr                =    zero
                     go to mcr-550.
       mcr-900.
      *              *-------------------------------------------------*
      *              * Ripristino linee 23 e 24 salvate in precedenza  *
      *              *                                                 *
      *              * Differenziato a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      w-s23                to   w-fld                  .
           move      23                   to   w-lin                  .
           move      01                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
           move      w-s24                to   w-fld                  .
           move      24                   to   w-lin                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Si riporta v-not con v-nt1 e v-nt2 a spaces     *
      *              *-------------------------------------------------*
           move      spaces               to   v-not                  .
       mcr-999.
           exit.

      *================================================================*
      *    Note alle righe 23 e 24                                     *
      *----------------------------------------------------------------*
       not-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione note a righe 23 e 24            *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      23                   to   w-lin                  .
           move      01                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           move      v-nt1                to   w-fld                  .
           perform   vis-mef-000          thru vis-mef-999            .
           move      24                   to   w-lin                  .
           move      v-nt2                to   w-fld                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Si riporta v-not con v-nt1 e v-nt2 a spaces     *
      *              *-------------------------------------------------*
           move      spaces               to   v-not                  .
       not-999.
           exit.

      *================================================================*
      *    Nota alla riga 23                                           *
      *----------------------------------------------------------------*
       no1-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione nota alla riga 23               *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      23                   to   w-lin                  .
           move      01                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           move      v-nt2                to   w-fld                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Si riporta v-not con v-nt1 e v-nt2 a spaces     *
      *              *-------------------------------------------------*
           move      spaces               to   v-not                  .
       no1-999.
           exit.

      *================================================================*
      * Nota alla riga 24                                              *
      *----------------------------------------------------------------*
       no2-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione nota alla riga 24               *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      24                   to   w-lin                  .
           move      01                   to   w-pos                  .
           move      z-mmx                to   w-siz                  .
           move      v-nt2                to   w-fld                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Si riporta v-not con v-nt1 e v-nt2 a spaces     *
      *              *-------------------------------------------------*
           move      spaces               to   v-not                  .
       no2-999.
           exit.

      *================================================================*
      * Refresh automatico                                             *
      *----------------------------------------------------------------*
       rau-000.
      *              *-------------------------------------------------*
      *              * Esecuzione del modo di utilizzo, solo se ne-    *
      *              * cessario                                        *
      *              *-------------------------------------------------*
           perform   xmx-000              thru xmx-999                .
      *              *-------------------------------------------------*
      *              * Erase screen                                    *
      *              *-------------------------------------------------*
           display   omitted              erase                       .
      *              *-------------------------------------------------*
      *              * Rivisualizzazione linee 01..24                  *
      *              *                                                 *
      *              * Differenziata a seconda se modo a 80 colonne o  *
      *              * a 132 colonne                                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
       rau-200.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    24
                     go to rau-999.
           display   b-lin(w-ctr)         line w-ctr
                                      position 01
                                          size z-mmx                  .
           go to     rau-200.
       rau-999.
           exit.

      *================================================================*
      * Refresh manuale, richiesto da operatore                        *
      *----------------------------------------------------------------*
       ref-000.
      *              *-------------------------------------------------*
      *              * Switch della modalita' grafica                  *
      *              *                                                 *
      *              * DISATTIVATO                                     *
      *              *-------------------------------------------------*
           go to     ref-100.
      *
           if        z-mdg                =    1
                     move  0              to   z-mdg
                     go to ref-100.
      *              *-------------------------------------------------*
      *              * Attivazione modalita' grafica                   *
      *              *-------------------------------------------------*
           move      1                    to   z-mdg                  .
       ref-100.
      *              *-------------------------------------------------*
      *              * Esecuzione del refresh automatico               *
      *              *-------------------------------------------------*
           perform   rau-000              thru rau-999                .
       ref-999.
           exit.

      *================================================================*
      * Desk-Accessory richiesto da operatore di tipo HELP             *
      *----------------------------------------------------------------*
       dac-hlp-000.
      *              *-------------------------------------------------*
      *              * Test su flag di desk-accessory abilitato o meno *
      *              *-------------------------------------------------*
           if        w-dac-prm-hlp-flg    not  = spaces
                     go to dac-hlp-999.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine a video                    *
      *              *-------------------------------------------------*
           perform   sav-000              thru sav-999                .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di interfaccia con il si-   *
      *              * stema operativo ospite                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      "DA"                 to   o-ope                  .
      *                  *---------------------------------------------*
      *                  * Tipo desk-accessory                         *
      *                  *                                             *
      *                  * Corrisponde alla variabile V_DKB_SUBT per   *
      *                  * il lancio di Tangram                        *
      *                  *---------------------------------------------*
           move      "HELP"               to   o-com                  .
      *                  *---------------------------------------------*
      *                  * Parametri per il desk-accessory             *
      *                  *                                             *
      *                  * Corrisponde alla variabile V_DKB_PRMS per   *
      *                  * il lancio di Tangram                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione iniziale                *
      *                      *-----------------------------------------*
           move      spaces               to   w-dac-prm-hlp          .
      *                      *-----------------------------------------*
      *                      * Record number attuale relativo al li-   *
      *                      * vello di Save/Restore immagine video    *
      *                      *-----------------------------------------*
           move      z-rcn                to   w-dac-prm-hlp-rcn      .
      *                      *-----------------------------------------*
      *                      * Modo di utilizzo attuale, se 80 o 132   *
      *                      * colonne                                 *
      *                      *-----------------------------------------*
           move      z-mod                to   w-dac-prm-hlp-zmo      .
      *                      *-----------------------------------------*
      *                      * Ultimo modo di utilizzo effettivamente  *
      *                      * attivato, se 80 o 132 colonne           *
      *                      *-----------------------------------------*
           move      z-umu                to   w-dac-prm-hlp-zum      .
      *                      *-----------------------------------------*
      *                      * Parametro in area di link               *
      *                      *-----------------------------------------*
           move      w-dac-prm-hlp        to   o-pat                  .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Modo di utilizzo attuale a 'Indefinito', per    *
      *              * forzare comunque l'emissione della sequenza     *
      *              * di ripristino del modo di utilizzo              *
      *              *-------------------------------------------------*
           move      9                    to   z-mod                  .
      *              *-------------------------------------------------*
      *              * Ultimo modo di utilizzo effettivamente attivato *
      *              * a 'Indefinito', per forzare comunque l'emissio- *
      *              * ne della sequenza di ripristino del modo di u-  *
      *              * tilizzo                                         *
      *              *-------------------------------------------------*
           move      9                    to   z-umu                  .
      *              *-------------------------------------------------*
      *              * Buffer immagine a video a high-values per for-  *
      *              * zare comunque la rivisualizzazione              *
      *              *-------------------------------------------------*
           move      high-values          to   b                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine a video                     *
      *              *-------------------------------------------------*
           perform   rst-000              thru rst-999                .
       dac-hlp-999.
           exit.

      *    *===========================================================*
      *    * Desk-Accessory richiesto da operatore di tipo SHCP        *
      *    *-----------------------------------------------------------*
       dac-shc-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine a video                    *
      *              *-------------------------------------------------*
           perform   sav-000              thru sav-999                .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di interfaccia con il si-   *
      *              * stema operativo ospite                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      "DA"                 to   o-ope                  .
      *                  *---------------------------------------------*
      *                  * Tipo desk-accessory                         *
      *                  *                                             *
      *                  * Corrisponde alla variabile V_DKB_SUBT per   *
      *                  * il lancio di Tangram                        *
      *                  *---------------------------------------------*
           move      "SHCP"               to   o-com                  .
      *                  *---------------------------------------------*
      *                  * Parametri per il desk-accessory             *
      *                  *                                             *
      *                  * Corrisponde alla variabile V_DKB_PRMS per   *
      *                  * il lancio di Tangram                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione iniziale                *
      *                      *-----------------------------------------*
           move      spaces               to   w-dac-prm-shc          .
      *                      *-----------------------------------------*
      *                      * Record number attuale relativo al li-   *
      *                      * vello di Save/Restore immagine video    *
      *                      *-----------------------------------------*
           move      z-rcn                to   w-dac-prm-shc-rcn      .
      *                      *-----------------------------------------*
      *                      * Modo di utilizzo attuale, se 80 o 132   *
      *                      * colonne                                 *
      *                      *-----------------------------------------*
           move      z-mod                to   w-dac-prm-shc-zmo      .
      *                      *-----------------------------------------*
      *                      * Ultimo modo di utilizzo effettivamente  *
      *                      * attivato, se 80 o 132 colonne           *
      *                      *-----------------------------------------*
           move      z-umu                to   w-dac-prm-shc-zum      .
      *                      *-----------------------------------------*
      *                      * Parametro in area di link               *
      *                      *-----------------------------------------*
           move      w-dac-prm-shc        to   o-pat                  .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Modo di utilizzo attuale a 'Indefinito', per    *
      *              * forzare comunque l'emissione della sequenza     *
      *              * di ripristino del modo di utilizzo              *
      *              *-------------------------------------------------*
           move      9                    to   z-mod                  .
      *              *-------------------------------------------------*
      *              * Ultimo modo di utilizzo effettivamente attivato *
      *              * a 'Indefinito', per forzare comunque l'emissio- *
      *              * ne della sequenza di ripristino del modo di u-  *
      *              * tilizzo                                         *
      *              *-------------------------------------------------*
           move      9                    to   z-umu                  .
      *              *-------------------------------------------------*
      *              * Buffer immagine a video a high-values per for-  *
      *              * zare comunque la rivisualizzazione              *
      *              *-------------------------------------------------*
           move      high-values          to   b                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine a video                     *
      *              *-------------------------------------------------*
           perform   rst-000              thru rst-999                .
       dac-shc-999.
           exit.

      *    *===========================================================*
      *    * Convert                                                   *
      *    *-----------------------------------------------------------*
       cvt-000.
           move      v-alf                to   w-fld                  .
           move      zero                 to   w-ctr                  .
           move      zero                 to   w-int                  .
           move      zero                 to   w-dec                  .
           move      zero                 to   w-dim                  .
           move      spaces               to   w-sgn                  .
       cvt-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    v-car
                     go to cvt-300.
           if        w-chr (w-ctr)        =    ","
                     go to cvt-200.
           if        w-chr (w-ctr)        =    "-"
                     move  "#"            to   w-sgn
                     go to cvt-100.
           if        w-chr (w-ctr)        <    "0" or
                     w-chr (w-ctr)        >    "9"
                     go to cvt-100.
           multiply  10                   by   w-int                  .
           add       w-chn (w-ctr)        to   w-int                  .
           go to     cvt-100.
       cvt-200.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    v-car
                     go to cvt-300.
           if        w-chr (w-ctr)        <    "0" or
                     w-chr (w-ctr)        >    "9"
                     go to cvt-200.
           add       1                    to   w-dim                  .
           move      w-chr (w-ctr)        to   w-dcc (w-dim)          .
           if        w-dim                =    5
                     go to cvt-300
           else      go to cvt-200.
       cvt-300.
            move     w-dec                to   v-num                  .
            divide   100000               into v-num                  .
            add      w-int                to   v-num                  .
            if       w-sgn                not  = spaces
                     multiply -1          by   v-num                  .
            move     w-dim                to   v-dec                  .
       cvt-999.
           exit.

      *================================================================*
      *    Sub-routines                                                *
      *================================================================*
           
      *================================================================*
      *    Display alfa                                                *
      *----------------------------------------------------------------*
       dsp-alf-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione      *
      *              *-------------------------------------------------*
           move      v-alf                to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-car                to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Preparazione campo editato e sua lunghezza      *
      *              *-------------------------------------------------*
           move      v-alf                to   v-edt                  .
           move      v-car                to   v-edl                  .
       dsp-alf-999.
           exit.

      *================================================================*
      *    Display text                                                *
      *----------------------------------------------------------------*
       dsp-txt-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione      *
      *              *-------------------------------------------------*
           move      v-txt (1 : v-car)    to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-car                to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Preparazione campo editato e sua lunghezza      *
      *              *-------------------------------------------------*
           move      v-txt (1 : v-car)    to   v-edt                  .
           move      v-car                to   v-edl                  .
       dsp-txt-999.
           exit.

      *================================================================*
      *    Display espanso                                             *
      *----------------------------------------------------------------*
       dsp-esp-000.
      *              *-------------------------------------------------*
      *              * Subroutine di editing campo espanso             *
      *              *-------------------------------------------------*
           perform   edt-esp-000          thru edt-esp-999            .
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione      *
      *              *-------------------------------------------------*
           move      v-edt                to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-car                to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
       dsp-esp-999.
           exit.

      *================================================================*
      *    Display check-box                                           *
      *----------------------------------------------------------------*
       dsp-ckb-000.
      *              *-------------------------------------------------*
      *              * Subroutine di editing campo check-box           *
      *              *-------------------------------------------------*
           perform   edt-ckb-000          thru edt-ckb-999            .
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione      *
      *              *-------------------------------------------------*
           move      v-edt                to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-edl                to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
       dsp-ckb-999.
           exit.

      *================================================================*
      *    Display numerico                                            *
      *----------------------------------------------------------------*
       dsp-num-000.
      *              *-------------------------------------------------*
      *              * Pre-intervento per tipo campo 'V''              *
      *              *-------------------------------------------------*
           perform   pre-icv-000          thru pre-icv-999            .
      *              *-------------------------------------------------*
      *              * Preparazione parametri per editing              *
      *              *-------------------------------------------------*
           move      v-num                to   w-num                  .
           perform   car-edn-000          thru car-edn-999            .
      *              *-------------------------------------------------*
      *              * Esecuzione editing numerico                     *
      *              *-------------------------------------------------*
           perform   num-edt-000          thru num-edt-999            .
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione     *
      *              *-------------------------------------------------*
           move      v-edt                to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-edl                to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Post-intervento per tipo campo 'V''             *
      *              *-------------------------------------------------*
           perform   pos-icv-000          thru pos-icv-999            .
       dsp-num-999.
           exit.

      *================================================================*
      *    Display progressivo/anno                                    *
      *----------------------------------------------------------------*
       dsp-pga-000.
      *              *-------------------------------------------------*
      *              * Esecuzione editing                              *
      *              *-------------------------------------------------*
           perform   edt-pga-000          thru edt-pga-999            .
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione      *
      *              *-------------------------------------------------*
           move      v-edt                to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-edl                to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
       dsp-pga-999.
           exit.

      *================================================================*
      *    Display data                                                *
      *----------------------------------------------------------------*
       dsp-dat-000.
      *              *-------------------------------------------------*
      *              * Preparazione data gg/mm/aa in w-dat             *
      *              *-------------------------------------------------*
           move      v-dat                to   w-amg                  .
           perform   dat-inv-000          thru dat-inv-999            .
      *              *-------------------------------------------------*
      *              * Esecuzione editing data                         *
      *              *-------------------------------------------------*
           perform   dat-edt-000          thru dat-edt-999            .
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione      *
      *              *-------------------------------------------------*
           move      v-edt                to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-edl                to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
       dsp-dat-999.
           exit.

      *================================================================*
      *    Display password                                            *
      *----------------------------------------------------------------*
       dsp-pwd-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione      *
      *              *-------------------------------------------------*
           move      spaces               to   w-fld                  .
           move      zero                 to   w-ctr                  .
       dsp-pwd-200.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    v-car
                     go to dsp-pwd-300.
           if        v-alf
                    (w-ctr : 1)           not  = spaces
                     move  "*"            to   v-alf
                                              (w-ctr : 1)             .
           go to     dsp-pwd-200.
       dsp-pwd-300.
           move      v-alf                to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-car                to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Preparazione campo editato e sua lunghezza      *
      *              *-------------------------------------------------*
           move      v-alf                to   v-edt                  .
           move      v-car                to   v-edl                  .
       dsp-pwd-999.
           exit.

      *================================================================*
      *    Edit espanso                                                *
      *----------------------------------------------------------------*
       edt-esp-000.
      *              *-------------------------------------------------*
      *              * Determinazione se pre-espansione comunque ri-   *
      *              * chiesta                                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "X"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-txe-pee
           else      move   "X"           to   w-txe-pee              .
      *              *-------------------------------------------------*
      *              * Determinazione se ammesso elemento manuale o no *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "M"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-txe-man
           else      move   "M"           to   w-txe-man              .
      *              *-------------------------------------------------*
      *              * Determinazione se compressione comunque inibi-  *
      *              * ta, solo se non check-box                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "#"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-txe-nco
           else      move   "#"           to   w-txe-nco              .
           if        w-cte                =    "C"
                     move  spaces         to   w-txe-nco              .
       edt-esp-200.
      *              *-------------------------------------------------*
      *              * Determinazione se caratteri di accettazione am- *
      *              * messi                                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-txe-mej              .
           move      spaces               to   w-txe-mex              .
           move      v-msk                to   w-txe-mex              .
           if        w-txe-mex            =    spaces
                     move  spaces         to   w-txe-mef
           else      move  "#"            to   w-txe-mef              .
      *                  *---------------------------------------------*
      *                  * Test se possibilita' di accettazione anche  *
      *                  * dell'indice numerico                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-msk            tallying w-ctr
                     for    all "#"                                   .
           if        w-ctr                =    zero
                     go to edt-esp-400.
      *                  *---------------------------------------------*
      *                  * Alla tabella si aggiungono anche gli indici *
      *                  * numerici (max 9)                            *
      *                  *---------------------------------------------*
           inspect   v-msk           replacing all "#" by spaces      .
           move      zero                 to   w-ctr                  .
           inspect   v-msk            tallying w-ctr
                     for characters     before initial spaces         .
           move      w-ctr                to   w-txe-mej              .
           move      zero                 to   w-txe-mez              .
           move      zero                 to   w-txe-mew              .
       edt-esp-220.
           add       1                    to   w-txe-mez              .
           if        w-txe-mez            >    w-txe-mej
                     go to                edt-esp-400.
           move      w-txe-mez            to   w-txe-mew              .
           add       w-txe-mej            to   w-txe-mew              .
           move      w-txe-meh
                    (w-txe-mez)           to   w-txe-mer
                                              (w-txe-mew)             .
           go to     edt-esp-220.
       edt-esp-400.
      *              *-------------------------------------------------*
      *              * Lunghezza del campo editato in v-edl            *
      *              *-------------------------------------------------*
           move      v-car                to   v-edl                  .
      *              *-------------------------------------------------*
      *              * Valore del campo editato in v-edt               *
      *              *-------------------------------------------------*
           if        v-num                not  = zero
                     go to edt-esp-500.
      *                  *---------------------------------------------*
      *                  * Se indice zero :                            *
      *                  * - se elemento manuale non ammesso : spaces  *
      *                  * - altrimenti : valore del campo v-alf       *
      *                  *---------------------------------------------*
           if        w-txe-man            =    spaces
                     move  spaces         to   v-edt
           else      move  v-alf          to   v-edt                  .
           go to     edt-esp-999.
       edt-esp-500.
      *                  *---------------------------------------------*
      *                  * Se indice non zero                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Estrazione del valore dell'elemento da  *
      *                      * v-txt                                   *
      *                      *-----------------------------------------*
           move      v-num                to   w-pnt                  .
           subtract  1                    from w-pnt                  .
           multiply  v-car                by   w-pnt                  .
           add       1                    to   w-pnt                  .
           move      spaces               to   w-fld                  .
           unstring  v-txt                into w-fld
                                  with pointer w-pnt                  .
           move      v-car                to   w-pnt                  .
           add       1                    to   w-pnt                  .
           move      high-value           to   w-chr (w-pnt)          .
           inspect   w-fld           replacing characters
                                          by   spaces
                                         after initial high-value     .
           move      spaces               to   w-chr (w-pnt)          .
           move      w-fld                to   v-edt                  .
       edt-esp-999.
           exit.

      *================================================================*
      *    Edit check-box                                              *
      *----------------------------------------------------------------*
       edt-ckb-000.
      *              *-------------------------------------------------*
      *              * Lunghezza del campo editato in v-edl, calcolan- *
      *              * do i caratteri di separazione                   *
      *              *-------------------------------------------------*
           move      v-ldt                to   v-edl                  .
           multiply  2                    by   v-edl                  .
           add       1                    to   v-edl                  .
      *              *-------------------------------------------------*
      *              * Normalizzazione area editata                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-edt                  .
      *              *-------------------------------------------------*
      *              * Se presente la clausola 'Blank when spaces' e   *
      *              * tutto e' a spaces : uscita immediata            *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "B"                                   .
           if        w-ctr                >    zero and
                     v-alf                =    spaces
                     go to edt-ckb-999.
       edt-ckb-100.
      *              *-------------------------------------------------*
      *              * Composizione area editata                       *
      *              *-------------------------------------------------*
       edt-ckb-200.
      *                  *---------------------------------------------*
      *                  * Premessa                                    *
      *                  *---------------------------------------------*
           move      "["                  to   v-edt (01 : 01)        .
       edt-ckb-300.
      *                  *---------------------------------------------*
      *                  * Ciclo su singoli caratteri                  *
      *                  *---------------------------------------------*
       edt-ckb-305.
      *                      *-----------------------------------------*
      *                      * Inizializzazione contatore su singoli   *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      zero                 to   w-ctr                  .
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice su singoli ca-  *
      *                      * ratteri editati                         *
      *                      *-----------------------------------------*
           move      1                    to   w-inx                  .
       edt-ckb-310.
      *                      *-----------------------------------------*
      *                      * Incremento contatore su singoli carat-  *
      *                      * teri                                    *
      *                      *-----------------------------------------*
           add       1                    to   w-ctr                  .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-ctr                >    v-ldt
                     go to edt-ckb-999.
      *                      *-----------------------------------------*
      *                      * Incremento indice su singoli caratteri  *
      *                      * editati                                 *
      *                      *-----------------------------------------*
           add       1                    to   w-inx                  .
      *                      *-----------------------------------------*
      *                      * Editing del carattere                   *
      *                      *-----------------------------------------*
           if        v-alf (w-ctr : 01)   =    spaces
                     move  spaces         to   v-edt (w-inx : 01)
           else      move  "X"            to   v-edt (w-inx : 01)     .
      *                      *-----------------------------------------*
      *                      * Incremento indice su singoli caratteri  *
      *                      * editati                                 *
      *                      *-----------------------------------------*
           add       1                    to   w-inx                  .
      *                      *-----------------------------------------*
      *                      * Editing del separatore                  *
      *                      *-----------------------------------------*
           if        w-ctr                =    v-ldt
                     move  "]"            to   v-edt (w-inx : 01)
           else      move  "|"            to   v-edt (w-inx : 01)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su singolo carattere successivo *
      *                      *-----------------------------------------*
           go to     edt-ckb-310.
       edt-ckb-999.
           exit.

      *================================================================*
      *    Edit numerico                                               *
      *----------------------------------------------------------------*
       edt-num-000.
      *              *-------------------------------------------------*
      *              * Pre-intervento per tipo campo 'V''              *
      *              *-------------------------------------------------*
           perform   pre-icv-000          thru pre-icv-999            .
      *              *-------------------------------------------------*
      *              * Preparazione campo numerico in w-num            *
      *              *-------------------------------------------------*
           move      v-num                to   w-num                  .
      *              *-------------------------------------------------*
      *              * Preparazione caratteristiche editing numerico   *
      *              *-------------------------------------------------*
           perform   car-edn-000          thru car-edn-999            .
      *              *-------------------------------------------------*
      *              * Editing vero e proprio da w-num                 *
      *              *-------------------------------------------------*
           perform   num-edt-000          thru num-edt-999            .
      *              *-------------------------------------------------*
      *              * Post-intervento per tipo campo 'V''             *
      *              *-------------------------------------------------*
           perform   pos-icv-000          thru pos-icv-999            .
       edt-num-999.
           exit.

      *================================================================*
      *    Edit progressivo/anno                                       *
      *----------------------------------------------------------------*
       edt-pga-000.
      *              *-------------------------------------------------*
      *              * Determinazione moltiplicatore divisore          *
      *              *-------------------------------------------------*
           move      1                    to   w-pmd                  .
           move      v-car                to   w-ctr                  .
       edt-pga-200.
           if        w-ctr                >    zero
                     multiply 10          by   w-pmd
                     subtract 1           from w-ctr
                     go to    edt-pga-200.
      *              *-------------------------------------------------*
      *              * Separazione campo numerico v-num in componenti  *
      *              * - v-sec : secolo (*)                            *
      *              * - v-ann : anno                                  *
      *              * - w-prn : numero progressivo                    *
      *              * ----------------------------------------------- *
      *              *                                                 *
      *              * (*) La normalizzazione del secolo nella data    *
      *              *     avviene come segue : se l'anno e' minore di *
      *              *     85 si assume il secolo relativo al 2000,    *
      *              *     altrimenti si assume il secolo relativo al  *
      *              *     1900. Se pero' l'intera data in formato     *
      *              *     'aa.mm.gg' e' a zero, sipone a zero anche   *
      *              *     il secolo                                   *
      *              *                                                 *
      *              *-------------------------------------------------*
           move      v-num                to   w-int                  .
           divide    w-pmd                into w-int
                                        giving w-ctr
                                     remainder w-prn                  .
           move      w-ctr                to   v-ann                  .
           if        w-prn                =    zero and
                     v-ann                =    zero
                     move  zero           to   v-sec
           else      if    v-ann          <    85
                           move   1       to   v-sec
                     else  move   zero    to   v-sec                  .
      *              *-------------------------------------------------*
      *              * Abblencamento rappresentazione editata totale   *
      *              *-------------------------------------------------*
           move      spaces               to   w-ped                  .
      *              *-------------------------------------------------*
      *              * Se tutto a zero : omette ulteriore editing      *
      *              *-------------------------------------------------*
           if        w-prn                =    zero and
                     v-ann                =    zero
                     go to edt-pga-400.
      *              *-------------------------------------------------*
      *              * Editing di : progressivo , barra , anno         *
      *              *-------------------------------------------------*
           move      w-prn                to   w-ped-pro              .
           move      "/"                  to   w-ped-bar              .
           move      v-ann                to   w-ped-ann              .
       edt-pga-400.
      *              *-------------------------------------------------*
      *              * Lunghezza campo editato in uscita               *
      *              *-------------------------------------------------*
           add       3
                     v-car              giving v-edl                  .
      *              *-------------------------------------------------*
      *              * Valore editato in uscita                        *
      *              *-------------------------------------------------*
           move      spaces               to   v-edt                  .
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "<"                                   .
           if        w-ctr                =    zero
                     subtract v-car       from 11
                                        giving w-pnt
                     go to    edt-pga-600.
           move      zero                 to   w-pnt                  .
           inspect   w-ped            tallying w-pnt
                                   for leading spaces                 .
           add       1                    to   w-pnt                  .
       edt-pga-600.
           unstring  w-ped                into v-edt
                                  with pointer w-pnt                  .
       edt-pga-999.
           exit.

      *================================================================*
      *    Edit data                                                   *
      *----------------------------------------------------------------*
       edt-dat-000.
      *              *-------------------------------------------------*
      *              * Preparazione data gg/mm/aa in w-dat             *
      *              *-------------------------------------------------*
           move      v-dat                to   w-amg                  .
           perform   dat-inv-000          thru dat-inv-999            .
      *              *-------------------------------------------------*
      *              * Editing vero e proprio da w-dat                 *
      *              *-------------------------------------------------*
           perform   dat-edt-000          thru dat-edt-999            .
       edt-dat-999.
           exit.

      *================================================================*
      *    Accept alfa                                                 *
      *----------------------------------------------------------------*
       acc-alf-000.
      *              *-------------------------------------------------*
      *              * Tipo di accettazione alfanumerica : normale     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cta                  .
      *              *-------------------------------------------------*
      *              * Accettazione campo di tipo alfanumerico         *
      *              *-------------------------------------------------*
           perform   acc-cta-000          thru acc-cta-999            .
       acc-alf-999.
           exit.

      *================================================================*
      *    Accettazione campo di tipo alfanumerico                     *
      *----------------------------------------------------------------*
       acc-cta-000.
      *              *-------------------------------------------------*
      *              * Se accettazione per check-box si abilitano, se  *
      *              * non gia' abilitati, i tasti Find ed Expd, e si  *
      *              * bufferizzano                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo accettazione                   *
      *                  *---------------------------------------------*
           if        w-cta                not  = "C"
                     go to acc-cta-050.
      *                  *---------------------------------------------*
      *                  * Find                                        *
      *                  *---------------------------------------------*
           set       v-pfx                to   1                      .
           search    v-pfk
                     when   v-pfk
                           (v-pfx)        =    spaces
                            move  "FIND"  to   v-pfk
                                              (v-pfx)                 .
      *                  *---------------------------------------------*
      *                  * Expd                                        *
      *                  *---------------------------------------------*
           set       v-pfx                to   1                      .
           search    v-pfk
                     when   v-pfk
                           (v-pfx)        =    spaces
                            move  "EXPD"  to   v-pfk
                                              (v-pfx)                 .
       acc-cta-050.
      *              *-------------------------------------------------*
      *              * Memorizzazione valore di default in work-area   *
      *              *-------------------------------------------------*
           move      v-alf                to   w-fld                  .
      *              *-------------------------------------------------*
      *              * Segnale di Insert Character Off                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-ich                  .
       acc-cta-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione indicatore carattere attuale   *
      *              *-------------------------------------------------*
           move      1                    to   w-ccr                  .
      *              *-------------------------------------------------*
      *              * Segnale prima impostazione                      *
      *              *-------------------------------------------------*
           move      "*"                  to   w-frs                  .
       acc-cta-150.
      *              *-------------------------------------------------*
      *              * Preparazione parametri in work-area             *
      *              *-------------------------------------------------*
           move      v-car                to   w-siz                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
      *              *-------------------------------------------------*
      *              * Visualizzazione default con underscores         *
      *              *-------------------------------------------------*
           move      w-fld                to   w-svf                  .
           inspect   w-fld           replacing all spaces by w-unu    .
           perform   vis-mef-000          thru vis-mef-999            .
           move      w-svf                to   w-fld                  .
           add       w-ccr                to   w-pos                  .
           subtract  1                    from w-pos                  .
       acc-cta-200.
      *              *-------------------------------------------------*
      *              * Se impostazione di un check-box e si e' sul 1.  *
      *              * carattere, ci si porta sul 2. carattere         *
      *              *-------------------------------------------------*
           if        w-cta                =    "C" and
                     w-pos                =    v-pos
                     add   1              to   w-pos
                     add   1              to   w-ccr                  .
      *              *-------------------------------------------------*
      *              * Salvataggio carattere precedente                *
      *              *-------------------------------------------------*
           move      w-chr(w-ccr)         to   w-sav                  .
      *              *-------------------------------------------------*
      *              * Se in Insert Character                          *
      *              *-------------------------------------------------*
           if        w-ich                =    spaces
                     go to acc-cta-230.
           move      spaces               to   w-txe-sva              .
           move      w-ccr                to   w-txe-stp              .
           unstring  w-fld                into w-txe-sva
                                  with pointer w-txe-stp              .
       acc-cta-230.
      *              *-------------------------------------------------*
      *              * Impostazione carattere attuale e determinazione *
      *              * del tasto di terminazione usato                 *
      *              *-------------------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
      *              *-------------------------------------------------*
      *              * Caso : No terminatore - impostato il carattere  *
      *              *-------------------------------------------------*
           if        k-key                not  = spaces
                     go to acc-cta-400.
      *                  *---------------------------------------------*
      *                  * Test se uppercase o lowercase o normale     *
      *                  *---------------------------------------------*
           if        v-tip                =    "U"
                     go to acc-cta-240
           else if   v-tip                =    "L"
                     go to acc-cta-260
           else      go to acc-cta-280.
       acc-cta-240.
      *                     *------------------------------------------*
      *                     * Se uppercase                             *
      *                     *------------------------------------------*
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-chr(w-ccr)   .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-chr(w-ccr)           .
           go to     acc-cta-280.
       acc-cta-260.
      *                     *------------------------------------------*
      *                     * Se lowercase                             *
      *                     *------------------------------------------*
           move      zero                 to   w-ulc                  .
           inspect   w-upp            tallying w-ulc
                     for characters     before initial w-chr(w-ccr)   .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-loc(w-ulc) to   w-chr(w-ccr)           .
       acc-cta-280.
      *                     *------------------------------------------*
      *                     * Se in impostazione per check-box si nor- *
      *                     * malizza a 'X' oppure a Spaces            *
      *                     *------------------------------------------*
           if        w-cta                =    "C"
                     if    w-chr(w-ccr)   =    spaces
                           move   spaces  to   w-chr (w-ccr)
                     else  move   "X"     to   w-chr (w-ccr)          .
      *                     *------------------------------------------*
      *                     * Visualizzazione carattere impostato      *
      *                     *------------------------------------------*
           perform   vis-mec-000          thru vis-mec-999            .
      *                     *------------------------------------------*
      *                     * Se in Insert Character                   *
      *                     *------------------------------------------*
           if        w-ich                =    spaces
                     go to acc-cta-290.
           move      w-ccr                to   w-txe-stp              .
           add       1                    to   w-txe-stp              .
           string    w-txe-sva  delimited by   size
                                          into w-fld
                                  with pointer w-txe-stp              .
           move      v-pos                to   w-pos                  .
           move      w-fld                to   w-svf                  .
           inspect   w-fld           replacing all spaces by w-unu    .
           perform   vis-mef-000          thru vis-mef-999            .
           move      w-svf                to   w-fld                  .
           add       w-ccr                to   w-pos                  .
           subtract  1                    from w-pos                  .
       acc-cta-290.
      *                     *------------------------------------------*
      *                     * Tests se primo carattere impostato, a    *
      *                     * meno di non essere in accettazione di    *
      *                     * tipo check-box                           *
      *                     *------------------------------------------*
           if        w-frs                not  = spaces and
                     w-cta                not  = "C"    and
                     w-ccr                =    1
                     move   spaces        to   w-frs
                     if     w-fld         not  = w-chr(1)
                            move   w-chr(w-ccr)
                                          to   w-fld
                            move   2      to   w-ccr
                            go to  acc-cta-150.
       acc-cta-300.
      *                     *------------------------------------------*
      *                     * Si avanza alla prossima posizione, a me- *
      *                     * no di non essere gia' all' ultima        *
      *                     *------------------------------------------*
           if        w-cta                =    "C"
                     go to acc-cta-310.
       acc-cta-305.
           if        w-ccr                not  = w-siz
                     add   1              to   w-ccr
                     add   1              to   w-pos                  .
           go to     acc-cta-200.
       acc-cta-310.
           move      w-ccr                to   w-ctr                  .
           add       2                    to   w-ctr                  .
           if        w-ctr                not  > w-siz
                     add   2              to   w-ccr
                     add   2              to   w-pos                  .
           go to     acc-cta-200.
       acc-cta-400.
      *              *-------------------------------------------------*
      *              * Ripristino carattere precedentemente salvato    *
      *              *-------------------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
      *              *-------------------------------------------------*
      *              * Segnale non piu' prima digitazione              *
      *              *-------------------------------------------------*
           move      spaces               to   w-frs                  .
      *              *-------------------------------------------------*
      *              * Segnale di Insert Character Off                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-ich                  .
      *              *-------------------------------------------------*
      *              * Test se function-key prevista                   *
      *              *-------------------------------------------------*
           perform   tst-pfk-000          thru tst-pfk-999            .
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Delete Character             *
      *              *-------------------------------------------------*
           if        k-key                not  = "DCHR"
                     go to acc-cta-410.
           if        w-cta                =    "C"
                     go to acc-cta-200.
           move      spaces               to   w-txe-sva              .
           move      w-ccr                to   w-txe-stp              .
           add       1                    to   w-txe-stp              .
           unstring  w-fld                into w-txe-sva
                                  with pointer w-txe-stp              .
           move      w-ccr                to   w-txe-stp              .
           string    w-txe-sva  delimited by   size
                                          into w-fld
                                  with pointer w-txe-stp              .
           move      v-pos                to   w-pos                  .
           go to     acc-cta-150.
       acc-cta-410.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Insert Character             *
      *              *-------------------------------------------------*
           if        k-key                not  = "ICHR"
                     go to acc-cta-490.
           if        w-cta                =    "C"
                     go to acc-cta-200.
           move      "#"                  to   w-ich                  .
           go to     acc-cta-200.
       acc-cta-490.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Left Arrow                   *
      *              *-------------------------------------------------*
           if        k-key                not  = "LEFT"
                     go to acc-cta-500.
       acc-cta-492.
      *                     *------------------------------------------*
      *                     * Deviazione a seconda se in accettazione  *
      *                     * check-box oppure no                      *
      *                     *------------------------------------------*
           if        w-cta                =    "C"
                     go to acc-cta-496.
       acc-cta-494.
      *                     *------------------------------------------*
      *                     * Accettazione normale                     *
      *                     *------------------------------------------*
      *                         *--------------------------------------*
      *                         * Se si e' in una posizione superiore  *
      *                         * alla prima si torna alla posizione   *
      *                         * precedente                           *
      *                         *--------------------------------------*
           if        w-ccr                not  = 1
                     subtract  1          from w-ccr
                                               w-pos
                     go to     acc-cta-200.
      *                         *--------------------------------------*
      *                         * Altrimenti se il contenuto attuale   *
      *                         * e' uguale al valore di default lo si *
      *                         * annulla, in caso contrario si ripri- *
      *                         * stina il valore di default           *
      *                         *--------------------------------------*
           if        w-fld                =    v-alf
                     move   spaces        to   w-fld
                     go to  acc-cta-100
           else      go to  acc-cta-050.
       acc-cta-496.
      *                     *------------------------------------------*
      *                     * Accettazione di check-box                *
      *                     *------------------------------------------*
      *                         *--------------------------------------*
      *                         * Se si e' in una posizione superiore  *
      *                         * alla seconda si torna indietro di    *
      *                         * due posizioni                        *
      *                         *--------------------------------------*
           if        w-ccr                >    2
                     subtract  2          from w-ccr
                     subtract  2          from w-pos                  .
           go to     acc-cta-200.
       acc-cta-500.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Right Arrow                  *
      *              *-------------------------------------------------*
           if        k-key                =    "RGHT"
                     go to acc-cta-300.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Append                       *
      *              *-------------------------------------------------*
           if        k-key                not  = "APND"
                     go to acc-cta-550.
           if        w-cta                =    "C"
                     go to acc-cta-200.
           move      v-car                to   w-ccr                  .
       acc-cta-510.
           if        w-chr(w-ccr)         =    spaces
                     if     w-ccr         >    1
                            subtract 1    from w-ccr
                            go to    acc-cta-510
                     else   go to    acc-cta-520.
           add       2                    to   w-ccr                  .
       acc-cta-520.
           if        w-ccr                >    v-car
                     move   v-car         to   w-ccr                  .
           add       v-pos
                     w-ccr              giving w-pos                  .
           subtract  1                    from w-pos                  .
           go to     acc-cta-200.
       acc-cta-550.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Return                       *
      *              *-------------------------------------------------*
           if        k-key                not  = "RTRN"
                     go to acc-cta-700.
      *                     *------------------------------------------*
      *                     * Preparazione tasto di fine in uscita     *
      *                     *------------------------------------------*
           move      spaces               to   v-key                  .
       acc-cta-600.
      *                     *------------------------------------------*
      *                     * Visualizzazione campo impostato ed ag-   *
      *                     * giornamento buffer screen corrispondente *
      *                     *------------------------------------------*
           move      v-pos                to   w-pos                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *                     *------------------------------------------*
      *                     * Va' all'uscita dalla routine             *
      *                     *------------------------------------------*
           go to     acc-cta-900.
       acc-cta-700.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore non riconosciuto             *
      *              *-------------------------------------------------*
           if        k-key                =    high-value
                     go to acc-cta-200.
       acc-cta-710.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore "COPY"                       *
      *              *-------------------------------------------------*
           if        k-key                not  = "COPY"
                     go to acc-cta-720.
           if        w-cta                =    "C"
                     go to acc-cta-200.
           move      w-fld                to   z-cla                  .
           go to     acc-cta-200.
       acc-cta-720.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore "PAST"                       *
      *              *-------------------------------------------------*
           if        k-key                not  = "PAST"
                     go to acc-cta-730.
           move      z-cla                to   w-fld                  .
           if        v-tip                =    "U"
                     perform upp-cas-000  thru upp-cas-999
           else if   v-tip                =    "L"
                     perform low-cas-000  thru low-cas-999            .
           go to     acc-cta-100.
       acc-cta-730.
      *              *-------------------------------------------------*
      *              * Caso : Altri terminatori riconosciuti           *
      *              *-------------------------------------------------*
           move      k-key                to   v-key                  .
           go to     acc-cta-600.
       acc-cta-900.
      *              *-------------------------------------------------*
      *              * Uscita da impostazione                          *
      *              *-------------------------------------------------*
      *                     *------------------------------------------*
      *                     * Soppressione high-values da w-fld        *
      *                     *------------------------------------------*
           inspect   w-fld  replacing     all  high-values by spaces  .
      *                     *------------------------------------------*
      *                     * Segnale se campo modificato              *
      *                     *------------------------------------------*
           if        w-fld                =    v-alf
                     move   spaces        to   v-mod
           else      move   "S"           to   v-mod                  .
      *                     *------------------------------------------*
      *                     * Preparazione campo in uscita             *
      *                     *------------------------------------------*
           move      w-fld                to   v-alf                  .
      *                     *------------------------------------------*
      *                     * Preparazione area di edit                *
      *                     *------------------------------------------*
           move      v-alf                to   v-edt                  .
           move      v-car                to   v-edl                  .
      *                     *------------------------------------------*
      *                     * Normalizzazione f-keys possibili         *
      *                     *------------------------------------------*
           move      spaces               to   v-ufk                  .
       acc-cta-999.
           exit.

      *================================================================*
      * Accept text                                                    *
      *----------------------------------------------------------------*
       acc-txt-000.
      *              *-------------------------------------------------*
      *              * Determinazione se richiesto Auto Wrap           *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "W"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-txe-awr
           else      move   "W"           to   w-txe-awr              .
      *              *-------------------------------------------------*
      *              * Segnale di Auto Wrap Off                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-txe-awf              .
      *              *-------------------------------------------------*
      *              * Calcolo numero totale di caratteri del campo    *
      *              *-------------------------------------------------*
           multiply  v-car                by   v-ldt
                                        giving w-txe-ntc              .
      *              *-------------------------------------------------*
      *              * Salvataggio valore di default in work-area      *
      *              *-------------------------------------------------*
           move      v-txt                to   w-txe-sav              .
      *              *-------------------------------------------------*
      *              * Segnale di Insert Character Off                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-ich                  .
      *              *-------------------------------------------------*
      *              * Salvataggio area video potenzialmente occupata  *
      *              * dal campo di tipo text in caso di espansione    *
      *              *-------------------------------------------------*
           perform   sav-txe-000          thru sav-txe-999            .
       acc-txt-025.
      *              *-------------------------------------------------*
      *              * Determinazione se richiesta apertura preventiva *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "X"                                   .
           if        w-ctr                >    zero
                     go to acc-txt-075.
       acc-txt-030.
      *              *-------------------------------------------------*
      *              * Se il campo contiene piu' di una linea si ese-  *
      *              * gue una espansione preventiva                   *
      *              *-------------------------------------------------*
           move      1                    to   w-ctr                  .
       acc-txt-050.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    v-ldt
                     go to acc-txt-100.
           move      w-ctr                to   w-pnt                  .
           subtract  1                    from w-pnt                  .
           multiply  v-car                by   w-pnt                  .
           add       1                    to   w-pnt                  .
           move      spaces               to   w-fld                  .
           unstring  v-txt                into w-fld
                                  with pointer w-pnt                  .
           move      v-car                to   w-pnt                  .
           add       1                    to   w-pnt                  .
           move      high-value           to   w-chr (w-pnt)          .
           inspect   w-fld           replacing characters
                                          by   spaces
                                         after initial high-value     .
           move      spaces               to   w-chr (w-pnt)          .
           if        w-fld                =    spaces
                     go to acc-txt-050.
                     
                     
       acc-txt-075.
                     
                     
                     
           perform   exp-txe-000          thru exp-txe-999            .
           move      1                    to   w-ccr                  .
           move      "*"                  to   w-frs                  .
           go to     acc-txt-200.
       acc-txt-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione indicatore carattere attuale   *
      *              *-------------------------------------------------*
           move      1                    to   w-ccr                  .
      *              *-------------------------------------------------*
      *              * Segnale prima impostazione                      *
      *              *-------------------------------------------------*
           move      "*"                  to   w-frs                  .
       acc-txt-150.
      *              *-------------------------------------------------*
      *              * Eventuale compressione del campo text e visua-  *
      *              * lizzazione default con underscores              *
      *              *-------------------------------------------------*
           move      "U"                  to   w-txe-tdc              .
           perform   cmp-txe-000          thru cmp-txe-999            .
       acc-txt-200.
      *              *-------------------------------------------------*
      *              * Determinazione indice di linea e indice di po-  *
      *              * sizione in funzione del numero progressivo di   *
      *              * carattere del campo text rispettivamente in     *
      *              * w-txe-ixl e w-txe-ixp                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-ccr
                                        giving w-txe-ixl              .
           divide    v-car                into w-txe-ixl              .
           multiply  v-car                by   w-txe-ixl
                                        giving w-txe-wk1              .
           subtract  w-txe-wk1            from w-ccr
                                        giving w-txe-ixp              .
           add       1                    to   w-txe-ixl              .
      *              *-------------------------------------------------*
      *              * Se oltre la prima linea si esegue l'espansione  *
      *              * del campo se questo non e' gia' espanso         *
      *              *-------------------------------------------------*
           if        w-txe-ixl            >    1
                     perform exp-txe-000  thru exp-txe-999            .
      *              *-------------------------------------------------*
      *              * Determinazione della linea                      *
      *              *-------------------------------------------------*
           if        w-txe-exp            =    spaces
                     move  v-lin          to   w-lin
           else      add   w-txe-ixl
                           w-txe-stl    giving w-lin                  .
      *              *-------------------------------------------------*
      *              * Determinazione della posizione                  *
      *              *-------------------------------------------------*
           add       v-pos
                     w-txe-ixp          giving w-pos                  .
           subtract  1                    from w-pos                  .
      *              *-------------------------------------------------*
      *              * Salvataggio carattere precedente                *
      *              *-------------------------------------------------*
           move      v-txc(w-ccr)         to   w-sav                  .
      *              *-------------------------------------------------*
      *              * Salvataggio campo testo per trattamento Insert  *
      *              * Character                                       *
      *              *-------------------------------------------------*
           move      v-txt                to   w-txe-trt              .
       acc-txt-225.
      *              *-------------------------------------------------*
      *              * Esecuzione del modo di utilizzo, solo se ne-    *
      *              * cessario                                        *
      *              *-------------------------------------------------*
           perform   xmx-000              thru xmx-999                .
      *              *-------------------------------------------------*
      *              * Impostazione carattere                          *
      *              *-------------------------------------------------*
           accept    v-txc(w-ccr)         line w-lin
                                      position w-pos off no beep
                                          on   exception w-exc
                     go to acc-txt-228.
      *                  *---------------------------------------------*
      *                  * Se non c'e' exception in assoluto           *
      *                  *---------------------------------------------*
           if        w-exc                =    00
                     move  spaces         to   k-key
                     go to acc-txt-250.
      *                  *---------------------------------------------*
      *                  * Se eccezione non riconosciuta               *
      *                  *---------------------------------------------*
           move      high-value           to   k-key                  .
           go to     acc-txt-250.
       acc-txt-228.
      *                  *---------------------------------------------*
      *                  * Se c'e' exception                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione tasto di terminazione    *
      *                      *-----------------------------------------*
           search    all     k-ele 
                     when    k-val(k-inx) =    w-exc
                     move    k-fnc(k-inx) to   k-key
                     go to   acc-txt-230.
      *                      *-----------------------------------------*
      *                      * Se non trovato : high-value             *
      *                      *-----------------------------------------*
           move      high-value           to   k-key                  .
           go to     acc-txt-250.
       acc-txt-230.
      *                      *-----------------------------------------*
      *                      * Se Desk-Accessory                       *
      *                      *-----------------------------------------*
       acc-txt-231.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        k-key                =    "HELP"
                     go to acc-txt-232
           else if   k-key                =    "SHCP"
                     go to acc-txt-233
           else      go to acc-txt-240.
       acc-txt-232.
      *                          *-------------------------------------*
      *                          * Se di tipo HELP                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Desk-Accessory Tipo  *
      *                              * Help                            *
      *                              *---------------------------------*
           perform   dac-hlp-000          thru dac-hlp-999            .
      *                              *---------------------------------*
      *                              * Key a High-Value                *
      *                              *---------------------------------*
           move      high-value           to   k-key                  .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     acc-txt-250.
       acc-txt-233.
      *                          *-------------------------------------*
      *                          * Se di tipo SHCP                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Desk-Accessory tipo  *
      *                              * Screen-Hard-Copy                *
      *                              *---------------------------------*
           perform   dac-shc-000          thru dac-shc-999            .
      *                              *---------------------------------*
      *                              * Key a High-Value                *
      *                              *---------------------------------*
           move      high-value           to   k-key                  .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     acc-txt-250.
       acc-txt-240.
      *                      *-----------------------------------------*
      *                      * Se Refresh                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        k-key                not  = "RFSH"
                     go to acc-txt-250.
      *                          *-------------------------------------*
      *                          * Esecuzione Refresh                  *
      *                          *-------------------------------------*
           perform   ref-000              thru ref-999                .
      *                          *-------------------------------------*
      *                          * Key a High-Value                    *
      *                          *-------------------------------------*
           move      high-value           to   k-key                  .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-txt-250.
       acc-txt-250.
      *              *-------------------------------------------------*
      *              * Caso : No terminatore - impostato il carattere  *
      *              *-------------------------------------------------*
           if        k-key                not  = spaces
                     go to acc-txt-400.
      *                  *---------------------------------------------*
      *                  * Test se primo carattere impostato           *
      *                  *---------------------------------------------*
           if        w-frs                not  = spaces and
                     w-ccr                =    1
                     move   spaces        to   w-frs
                     if     v-txt         not  = v-txc(w-ccr)
                            move   v-txc(w-ccr)
                                          to   v-txt
                            move   2      to   w-ccr
                            go to  acc-txt-150.
      *                  *---------------------------------------------*
      *                  * Test se richiesto Auto Wrap                 *
      *                  *---------------------------------------------*
           if        w-txe-awr            not  = "W"
                     go to acc-txt-290.
      *                  *---------------------------------------------*
      *                  * Test se Auto Wrap attivo                    *
      *                  *---------------------------------------------*
           if        w-txe-awf            not  = spaces
                     move  spaces         to   w-txe-awf
                     go to acc-txt-290.
      *                  *---------------------------------------------*
      *                  * Test se linea successiva alla prima         *
      *                  *---------------------------------------------*
           if        w-txe-ixl            =    001
                     go to acc-txt-290.
      *                  *---------------------------------------------*
      *                  * Test se carattere impostato di inizio linea *
      *                  *---------------------------------------------*
           if        w-txe-ixp            not  = 001
                     go to acc-txt-290.
      *                  *---------------------------------------------*
      *                  * Test su carattere precedente                *
      *                  *---------------------------------------------*
           move      w-ccr                to   w-txe-wk1              .
           subtract  1                    from w-txe-wk1              .
           if        v-txc(w-txe-wk1)     =    " "  or
                     v-txc(w-txe-wk1)     =    "-"  or
                     v-txc(w-txe-wk1)     =    "+"  or
                     v-txc(w-txe-wk1)     =    "/"  or
                     v-txc(w-txe-wk1)     =    "|"  or
                     v-txc(w-txe-wk1)     =    "."  or
                     v-txc(w-txe-wk1)     =    ","  or
                     v-txc(w-txe-wk1)     =    ";"  or
                     v-txc(w-txe-wk1)     =    ":"  or
                     v-txc(w-txe-wk1)     =    "!"  or
                     v-txc(w-txe-wk1)     =    "?"  or
                     v-txc(w-txe-wk1)     =    "^"  or
                     v-txc(w-txe-wk1)     =    "*"  or
                     v-txc(w-txe-wk1)     =    "#"  or
                     v-txc(w-txe-wk1)     =    "@"  or
                     v-txc(w-txe-wk1)     =    """"
                     go to acc-txt-290.
      *                  *---------------------------------------------*
      *                  * Test se impostato un blank                  *
      *                  *---------------------------------------------*
           if        v-txc(w-ccr)         =    " "
                     subtract 1           from w-ccr
                     move  "#"            to   w-txe-awf
                     go to acc-txt-295.
      *                  *---------------------------------------------*
      *                  * Funzione di Auto Wrap                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione del primo blank prece-   *
      *                      * dente il carattere impostato            *
      *                      *-----------------------------------------*
           move      w-ccr                to   w-txe-wk1              .
       acc-txt-270.
           subtract  1                    from w-txe-wk1              .
           if        w-txe-wk1            =    zero
                     go to acc-txt-275.
           if        v-txc(w-txe-wk1)     not  = spaces
                     go to acc-txt-270.
       acc-txt-275.
      *                      *-----------------------------------------*
      *                      * Determinazione lunghezza stringa da     *
      *                      * spostare                                *
      *                      *-----------------------------------------*
           if        w-ccr                not  > w-txe-wk1
                     go to acc-txt-290.
           subtract  w-txe-wk1            from w-ccr
                                        giving w-txe-wk2              .
           subtract  1                    from w-txe-wk2              .
      *                      *-----------------------------------------*
      *                      * Spostamento del testo a partire dallo   *
      *                      * spazio rilevato per un numero di carat- *
      *                      * teri pari alla lunghezza della stringa  *
      *                      * da spostare                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Bufferizzazione stringa da riporta- *
      *                          * re alla linea successiva            *
      *                          *-------------------------------------*
           move      w-txe-wk1            to   w-txe-wk3              .
           add       1                    to   w-txe-wk3              .
           move      v-txt
                    (w-txe-wk3 : w-txe-wk2)
                                          to   w-txe-wt3              .
      *                          *-------------------------------------*
      *                          * Abblencamento stringa da riportare  *
      *                          * alla linea successiva               *
      *                          *-------------------------------------*
           move      spaces               to   v-txt
                                              (w-txe-wk3 : w-txe-wk2) .
      *                          *-------------------------------------*
      *                          * Spostamento effettivo               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Posizione attuale in comodo     *
      *                              *---------------------------------*
           move      w-ccr                to   w-txe-wk4              .
      *                              *---------------------------------*
      *                              * Aggiunta della lunghezza della  *
      *                              * stringa da spostare             *
      *                              *---------------------------------*
           add       w-txe-wk2            to   w-txe-wk4              .
           add       1                    to   w-txe-wk4              .
      *                              *---------------------------------*
      *                              * Spostamento del contenuto del   *
      *                              * campo testo, a partire dalla    *
      *                              * posizione attuale del cursore,  *
      *                              * in un campo testo di comodo     *
      *                              *---------------------------------*
           move      v-txt
                    (w-ccr : 01)          to   w-new                  .
           move      w-sav                to   v-txt
                                              (w-ccr : 01)            .
           move      v-txt
                    (w-ccr : w-txe-ntc)   to   w-txe-trt              .
      *                              *---------------------------------*
      *                              * Tutta la stringa appena copiata *
      *                              * viene posta a partire dalla     *
      *                              * prima posizione libera dopo la  *
      *                              * stringa abbassata               *
      *                              *---------------------------------*
           move      w-txe-trt            to   v-txt
                                              (w-txe-wk4 : w-txe-ntc) .
      *                              *---------------------------------*
      *                              * Abbassamento della stringa      *
      *                              *---------------------------------*
           move      w-txe-wt3            to   v-txt
                                              (w-ccr     : w-txe-wk2) .
      *                              *---------------------------------*
      *                              * Aggiornamento posizione cursore *
      *                              *---------------------------------*
           move      w-txe-wk4            to   w-ccr                  .
      *                              *---------------------------------*
      *                              * Nuovo carattere impostato in    *
      *                              * campo di destinazione           *
      *                              *---------------------------------*
           move      w-txe-wk4            to   w-txe-wk5              .
           subtract  1                    from w-txe-wk5              .
           move      w-new                to   v-txt
                                              (w-txe-wk5 : 01)        .
      *                          *-------------------------------------*
      *                          * Subroutine di visualizzazione       *
      *                          *-------------------------------------*
           perform   acc-txt-awv-000      thru acc-txt-awv-999        .
      *                          *-------------------------------------*
      *                          * Attivazione Insert Character        *
      *                          *-------------------------------------*
           move      "#"                  to   w-ich                  .
      *                          *-------------------------------------*
      *                          * A riciclo su accettazione           *
      *                          *-------------------------------------*
           go to     acc-txt-200.
       acc-txt-290.
      *                  *---------------------------------------------*
      *                  * Visualizzazione carattere impostato         *
      *                  *---------------------------------------------*
           perform   vis-mct-000          thru vis-mct-999            .
       acc-txt-295.
      *                  *---------------------------------------------*
      *                  * Test se attivo Insert Character             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-ich                =    spaces
                     go to acc-txt-300.
      *                      *-----------------------------------------*
      *                      * Subroutine di scorrimento               *
      *                      *-----------------------------------------*
           perform   acc-txt-ics-000      thru acc-txt-ics-999        .
      *                      *-----------------------------------------*
      *                      * Subroutine di visualizzazione           *
      *                      *-----------------------------------------*
           perform   acc-txt-icv-000      thru acc-txt-icv-999        .
       acc-txt-300.
      *                  *---------------------------------------------*
      *                  * Si avanza al prossimo carattere, a meno di  *
      *                  * non essere gia' all'ultimo carattere        *
      *                  *---------------------------------------------*
           if        w-ccr                not  = w-txe-ntc
                     add   1              to   w-ccr                  .
      *                      *-----------------------------------------*
      *                      * A riciclo su accettazione               *
      *                      *-----------------------------------------*
           go to     acc-txt-200.
       acc-txt-400.
      *              *-------------------------------------------------*
      *              * Caso : Impostato un terminatore                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di Insert Character Off             *
      *                  *---------------------------------------------*
           move      spaces               to   w-ich                  .
      *                  *---------------------------------------------*
      *                  * Segnale non piu' prima digitazione          *
      *                  *---------------------------------------------*
           move      spaces               to   w-frs                  .
      *                  *---------------------------------------------*
      *                  * Ripristino carattere precedentem. salvato   *
      *                  *---------------------------------------------*
           move      w-sav                to   v-txc(w-ccr)           .
      *                  *---------------------------------------------*
      *                  * Test se function-key Expand                 *
      *                  *---------------------------------------------*
           if        k-key                =    "EXPD"
                     if     w-txe-exp     =    spaces
                            perform exp-txe-000
                               thru exp-txe-999
                            go to   acc-txt-200
                     else   move    "U"   to   w-txe-tdc
                            perform cmp-txe-000
                               thru cmp-txe-999
                            go to   acc-txt-100.
      *                  *---------------------------------------------*
      *                  * Test se function-key Up                     *
      *                  *---------------------------------------------*
           if        k-key                =    "UP  "   and
                     w-txe-exp            not  = spaces and
                     w-txe-ixl            not  = 1
                     subtract  v-car      from w-ccr
                     go to     acc-txt-200.
      *                  *---------------------------------------------*
      *                  * Test se function-key Down                   *
      *                  *---------------------------------------------*
           if        k-key                =    "DOWN"   and
                     w-txe-exp            not  = spaces and
                     w-txe-ixl            not  = v-ldt
                     add       v-car      to   w-ccr
                     go to     acc-txt-200.
      *                  *---------------------------------------------*
      *                  * Test se function-key prevista               *
      *                  *---------------------------------------------*
           perform   tst-pfk-000          thru tst-pfk-999            .
      *                  *---------------------------------------------*
      *                  * Caso : Terminatore Left Arrow               *
      *                  *---------------------------------------------*
           if        k-key                not  = "LEFT"
                     go to acc-txt-500.
      *                      *-----------------------------------------*
      *                      * Se si e' in una posizione superiore al- *
      *                      * la prima si torna alla posizione prece- *
      *                      * dente                                   *
      *                      *-----------------------------------------*
           if        w-ccr                not  = 1
                     subtract  1          from w-ccr
                     go to     acc-txt-200.
      *                      *-----------------------------------------*
      *                      * Altrimenti se il contenuto attuale e'   *
      *                      * uguale al valore di default lo si an-   *
      *                      * nulla, se invece e' diverso si ripri-   *
      *                      * stina il valore di default              *
      *                      *-----------------------------------------*
           if        v-txt                =    w-txe-sav
                     move  spaces         to   v-txt
           else      move  w-txe-sav      to   v-txt                  .
           go to     acc-txt-100.
       acc-txt-500.
      *                  *---------------------------------------------*
      *                  * Caso : Terminatore Right Arrow              *
      *                  *---------------------------------------------*
           if        k-key                =    "RGHT"
                     go to acc-txt-300.
      *                  *---------------------------------------------*
      *                  * Caso : Terminatore Append                   *
      *                  *---------------------------------------------*
           if        k-key                not  = "APND"
                     go to acc-txt-550.
           move      1                    to   w-txe-wpu              .
       acc-txt-510.
           if        w-txe-wpu            >    400
                     go to acc-txt-520.
           move      w-txe-wpu            to   w-ccr                  .
           unstring  v-txt      delimited by   all spaces
                                          into w-txe-wru
                                    count in   w-txe-wcu
                                  with pointer w-txe-wpu              .
           go to     acc-txt-510.
       acc-txt-520.
           add       w-txe-wcu            to   w-ccr                  .
           if        w-ccr                not  = 1
                     add   1              to   w-ccr                  .
           if        w-ccr                >    w-txe-ntc
                     move   w-txe-ntc     to   w-ccr                  .
           go to     acc-txt-200.
       acc-txt-550.
      *                  *---------------------------------------------*
      *                  * Caso : Terminatore Insert Character         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo terminatore                *
      *                      *-----------------------------------------*
           if        k-key                not  = "ICHR"
                     go to acc-txt-575.
      *                      *-----------------------------------------*
      *                      * Flag di Insert character attivo         *
      *                      *-----------------------------------------*
           move      "#"                  to   w-ich                  .
      *                      *-----------------------------------------*
      *                      * Espansione forzata del campo testo, se  *
      *                      * non e' gia' espanso                     *
      *                      *-----------------------------------------*
           if        w-txe-exp            =    spaces
                     perform exp-txe-000  thru exp-txe-999            .
      *                      *-----------------------------------------*
      *                      * Riciclo ad accettazione                 *
      *                      *-----------------------------------------*
           go to     acc-txt-200.
       acc-txt-575.
      *                  *---------------------------------------------*
      *                  * Caso : Terminatore Delete Character         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo terminatore                *
      *                      *-----------------------------------------*
           if        k-key                not  = "DCHR"
                     go to acc-txt-600.
      *                      *-----------------------------------------*
      *                      * Test che non si tratti dell'ultimo ca-  *
      *                      * rattere                                 *
      *                      *-----------------------------------------*
           if        w-ccr                not  = w-txe-ntc
                     go to acc-txt-580.
           go to     acc-txt-200.
       acc-txt-580.
      *                      *-----------------------------------------*
      *                      * Subroutine di trattamento               *
      *                      *-----------------------------------------*
           perform   acc-txt-dch-000      thru acc-txt-dch-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo ad accettazione                 *
      *                      *-----------------------------------------*
           go to     acc-txt-200.
       acc-txt-600.
      *                  *---------------------------------------------*
      *                  * Caso : Terminatore Return                   *
      *                  *---------------------------------------------*
           if        k-key                =    "RTRN"
                     move  spaces         to   v-key
                     go to acc-txt-900.
       acc-txt-700.
      *                  *---------------------------------------------*
      *                  * Caso : Terminatore non riconosciuto         *
      *                  *---------------------------------------------*
           if        k-key                =    "ICHR" or
                     k-key                =    "DCHR"
                     move  high-value     to   k-key                  .
           if        k-key                =    high-value
                     go to acc-txt-200.
      *                  *---------------------------------------------*
      *                  * Caso : Terminatore "COPY"                   *
      *                  *---------------------------------------------*
           if        k-key                =    "COPY"
                     move   v-txt         to   z-clt
                     go to  acc-txt-200.
      *                  *---------------------------------------------*
      *                  * Caso : Terminatore "PAST"                   *
      *                  *---------------------------------------------*
           if        k-key                =    "PAST"
                     move    z-clt        to   v-txt
                     go to  acc-txt-025.
      *                  *---------------------------------------------*
      *                  * Caso : Altri terminatori riconosciuti       *
      *                  *---------------------------------------------*
           move      k-key                to   v-key                  .
       acc-txt-900.
      *              *-------------------------------------------------*
      *              * Compressione del campo text e visualizzazione   *
      *              * dello stesso in forma senza underscores         *
      *              *-------------------------------------------------*
           move      "D"                  to   w-txe-tdc              .
           perform   cmp-txe-000          thru cmp-txe-999            .
      *              *-------------------------------------------------*
      *              * Segnale se campo modificato                     *
      *              *-------------------------------------------------*
           if        v-txt                =    w-txe-sav
                     move   spaces        to   v-mod
           else      move   "S"           to   v-mod                  .
      *              *-------------------------------------------------*
      *              * Normalizzazione f-keys possibili                *
      *              *-------------------------------------------------*
           move      spaces               to   v-ufk                  .
       acc-txt-999.
           exit.
           
      *================================================================*
      * Accept text                                                    *
      *                                                                *
      * Subroutine di Insert Character - Scorrimento                   *
      *----------------------------------------------------------------*
       acc-txt-ics-000.
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Carattere in corso di trattamento           *
      *                  *---------------------------------------------*
           move      w-ccr                to   w-txe-wkt              .
      *                  *---------------------------------------------*
      *                  * Puntatori                                   *
      *                  *---------------------------------------------*
           move      w-txe-ntc            to   w-txe-wcu              .
           move      w-txe-ntc            to   w-txe-wpu              .
           add       1                    to   w-txe-wpu              .
       acc-txt-ics-100.
      *                  *---------------------------------------------*
      *                  * Spostamento  a ritroso                      *
      *                  *---------------------------------------------*
           move      w-txe-cht(w-txe-wcu) to   v-txc(w-txe-wpu)       .
      *                  *---------------------------------------------*
      *                  * decremento contatori                        *
      *                  *---------------------------------------------*
           if        w-txe-wcu            >    w-txe-wkt
                     subtract   1         from w-txe-wcu
                     subtract   1         from w-txe-wpu
           else      go to acc-txt-ics-900.
       acc-txt-ics-200.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     acc-txt-ics-100.
       acc-txt-ics-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-txt-ics-999.
       acc-txt-ics-999.
           exit.

      *================================================================*
      *    Accept text                                                 *
      *                                                                *
      *    Subroutine di Insert Character - Visualizzazione            *
      *----------------------------------------------------------------*
       acc-txt-icv-000.
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione linea e posizione              *
      *                  *---------------------------------------------*
           move      w-txe-enl            to   w-txe-wkl              .
           subtract  1                    from w-txe-wkl              .
           move      w-txe-enp            to   w-txe-wkp              .
           subtract  1                    from w-txe-wkp              .
      *                  *---------------------------------------------*
      *                  * Start position, piu' uno                    *
      *                  *---------------------------------------------*
           move      w-txe-stp            to   w-txe-wka              .
           add       1                    to   w-txe-wka              .
      *                  *---------------------------------------------*
      *                  * End position, meno uno                      *
      *                  *---------------------------------------------*
           move      w-txe-enp            to   w-txe-wkz              .
           subtract  1                    from w-txe-wkz              .
      *                  *---------------------------------------------*
      *                  * Carattere in corso di trattamento, piu' uno *
      *                  *---------------------------------------------*
           move      w-ccr                to   w-txe-wkt              .
           add       1                    to   w-txe-wkt              .
      *                  *---------------------------------------------*
      *                  * Puntatori                                   *
      *                  *---------------------------------------------*
           move      w-txe-ntc            to   w-txe-wcu              .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ultimo carattere            *
      *                  *---------------------------------------------*
           move      v-txc(w-txe-wcu)     to   b-chr(w-txe-wkl,
                                                     w-txe-wkp)       .
           display   b-chr(w-txe-wkl,
                           w-txe-wkp)     line w-txe-wkl
                                      position w-txe-wkp              .
       acc-txt-icv-100.
      *                  *---------------------------------------------*
      *                  * Decremento puntatore                        *
      *                  *---------------------------------------------*
           if        w-txe-wcu            >    w-txe-wkt
                     subtract   1         from w-txe-wcu
           else      go to acc-txt-icv-900.
      *                  *---------------------------------------------*
      *                  * Determinazione di linea e posizione         *
      *                  *---------------------------------------------*
           if        w-txe-wkp            >    w-txe-wka
                     subtract 1           from w-txe-wkp
           else      move  w-txe-wkz      to   w-txe-wkp
                     subtract 1           from w-txe-wkl              .
      *                  *---------------------------------------------*
      *                  * Visualizzazione carattere                   *
      *                  *---------------------------------------------*
           move      v-txc(w-txe-wcu)     to   b-chr(w-txe-wkl,
                                                     w-txe-wkp)       .
           display   b-chr(w-txe-wkl,
                           w-txe-wkp)     line w-txe-wkl
                                      position w-txe-wkp              .
       acc-txt-icv-200.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     acc-txt-icv-100.
       acc-txt-icv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-txt-icv-999.
       acc-txt-icv-999.
           exit.

      *================================================================*
      *    Accept text                                                 *
      *                                                                *
      *    Subroutine di Delete Character                              *
      *----------------------------------------------------------------*
       acc-txt-dch-000.
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione della posizione in corso di *
      *                  * trattamento                                 *
      *                  *---------------------------------------------*
           move      w-lin                to   w-txe-wkl              .
           move      w-pos                to   w-txe-wkp              .
      *                  *---------------------------------------------*
      *                  * Start position, piu' uno                    *
      *                  *---------------------------------------------*
           move      w-txe-stp            to   w-txe-wka              .
           add       1                    to   w-txe-wka              .
      *                  *---------------------------------------------*
      *                  * End position, meno uno                      *
      *                  *---------------------------------------------*
           move      w-txe-enp            to   w-txe-wkz              .
           subtract  1                    from w-txe-wkz              .
      *                  *---------------------------------------------*
      *                  * Numero totale di caratteri                  *
      *                  *---------------------------------------------*
           move      w-txe-ntc            to   w-txe-wkt              .
           if        w-txe-exp            =    spaces
                     move  v-car          to   w-txe-wkt              .
      *                  *---------------------------------------------*
      *                  * Puntatori                                   *
      *                  *---------------------------------------------*
           move      w-ccr                to   w-txe-wcu              .
           add       1                    to   w-txe-wcu              .
           move      w-ccr                to   w-txe-wpu              .
           add       2                    to   w-txe-wpu              .
      *                  *---------------------------------------------*
      *                  * Spostamento a ritroso carattere in corso di *
      *                  * trattamento                                 *
      *                  *---------------------------------------------*
           move      v-txc(w-txe-wcu)     to   v-txc(w-ccr)           .
           move      v-txc(w-ccr)         to   b-chr(w-lin,w-pos)     .
           display   b-chr(w-lin,w-pos)   line w-lin
                                      position w-pos                  .
       acc-txt-dch-100.
      *                  *---------------------------------------------*
      *                  * Spostamento a ritroso                       *
      *                  *---------------------------------------------*
           move      v-txc(w-txe-wpu)     to   v-txc(w-txe-wcu)       .
      *                  *---------------------------------------------*
      *                  * Determinazione di linea e posizione         *
      *                  *---------------------------------------------*
           if        w-txe-wkp            <    w-txe-wkz
                     add   1              to   w-txe-wkp
           else      move  w-txe-wka      to   w-txe-wkp
                     add   1              to   w-txe-wkl              .
      *                  *---------------------------------------------*
      *                  * Visualizzazione carattere                   *
      *                  *---------------------------------------------*
           move      v-txc(w-txe-wcu)     to   b-chr(w-txe-wkl,
                                                     w-txe-wkp)       .
           display   b-chr(w-txe-wkl,
                           w-txe-wkp)     line w-txe-wkl
                                      position w-txe-wkp              .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           if        w-txe-wcu            not  = w-txe-wkt
                     add   1              to   w-txe-wcu
                     add   1              to   w-txe-wpu
           else      move  spaces         to   v-txc(w-txe-wcu)
                     go to acc-txt-dch-900.
       acc-txt-dch-200.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     acc-txt-dch-100.
       acc-txt-dch-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-txt-dch-999.
       acc-txt-dch-999.
           exit.

      *================================================================*
      *    Accept text                                                 *
      *                                                                *
      *    Subroutine di Auto Wrap - Visualizzazione                   *
      *----------------------------------------------------------------*
       acc-txt-awv-000.
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione linea e posizione              *
      *                  *---------------------------------------------*
           move      w-txe-enl            to   w-txe-wkl              .
           subtract  1                    from w-txe-wkl              .
           move      w-txe-enp            to   w-txe-wkp              .
           subtract  1                    from w-txe-wkp              .
      *                  *---------------------------------------------*
      *                  * Start position, piu' uno                    *
      *                  *---------------------------------------------*
           move      w-txe-stp            to   w-txe-wka              .
           add       1                    to   w-txe-wka              .
      *                  *---------------------------------------------*
      *                  * End position, meno uno                      *
      *                  *---------------------------------------------*
           move      w-txe-enp            to   w-txe-wkz              .
           subtract  1                    from w-txe-wkz              .
      *                  *---------------------------------------------*
      *                  * Carattere in corso di trattamento, piu' uno *
      *                  *---------------------------------------------*
           move      w-txe-wka            to   w-txe-wkt              .
      *                  *---------------------------------------------*
      *                  * Puntatori                                   *
      *                  *---------------------------------------------*
           move      w-txe-ntc            to   w-txe-wcu              .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ultimo carattere            *
      *                  *---------------------------------------------*
           move      v-txc(w-txe-wcu)     to   b-chr(w-txe-wkl,
                                                     w-txe-wkp)       .
           display   b-chr(w-txe-wkl,
                           w-txe-wkp)     line w-txe-wkl
                                      position w-txe-wkp              .
       acc-txt-awv-100.
      *                  *---------------------------------------------*
      *                  * Decremento puntatore                        *
      *                  *---------------------------------------------*
           if        w-txe-wcu            >    w-txe-wkt
                     subtract   1         from w-txe-wcu
           else      go to acc-txt-awv-900.
      *                  *---------------------------------------------*
      *                  * Determinazione di linea e posizione         *
      *                  *---------------------------------------------*
           if        w-txe-wkp            >    w-txe-wka
                     subtract 1           from w-txe-wkp
           else      move  w-txe-wkz      to   w-txe-wkp
                     subtract 1           from w-txe-wkl              .
      *                  *---------------------------------------------*
      *                  * Visualizzazione carattere                   *
      *                  *---------------------------------------------*
           move      v-txc(w-txe-wcu)     to   b-chr(w-txe-wkl,
                                                     w-txe-wkp)       .
           display   b-chr(w-txe-wkl,
                           w-txe-wkp)     line w-txe-wkl
                                      position w-txe-wkp              .
       acc-txt-awv-200.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     acc-txt-awv-100.
       acc-txt-awv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-txt-awv-999.
       acc-txt-awv-999.
           exit.

      *================================================================*
      *    Salvataggio area video potenzialmente occupata dal campo di *
      *    tipo 'text' o 'espanso' in caso di espansione               *
      *----------------------------------------------------------------*
       sav-txe-000.
      *              *-------------------------------------------------*
      *              * Segnale di campo non espanso                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-txe-exp              .
      *              *-------------------------------------------------*
      *              * Salvataggio buffer 'b' in area 'w-txe-siv-dat'  *
      *              *-------------------------------------------------*
           move      b                    to   w-txe-siv-dat          .
      *              *-------------------------------------------------*
      *              * Determinazione della linea iniziale e della li- *
      *              * nea finale del campo espanso compresi i delimi- *
      *              * tatori                                          *
      *              *-------------------------------------------------*
           subtract  1                    from v-lin
                                        giving w-txe-stl              .
           add       v-ldt
                     v-lin              giving w-txe-enl              .
           if        w-txe-stl            =    zero
                     add   1              to   w-txe-stl
                                               w-txe-enl              .
           if        w-txe-enl            >    24
                     subtract  24         from w-txe-enl
                                        giving w-txe-wk1
                     subtract  w-txe-wk1  from w-txe-stl
                                               w-txe-enl              .
      *              *-------------------------------------------------*
      *              * Determinazione della posizione iniziale e della *
      *              * posizione finale per delimitatori campo espanso *
      *              *-------------------------------------------------*
           move      v-pos                to   w-txe-stp              .
           if        w-txe-stp            >    1
                     subtract 1           from w-txe-stp              .
           add       v-pos
                     v-car              giving w-txe-enp              .
           if        w-txe-enp            >    z-mmx
                     move  z-mmx          to   w-txe-enp              .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice iniziale e dell'in-  *
      *              * dice finale per i delimitatori campo espanso    *
      *              *-------------------------------------------------*
           if        v-pos                =    1
                     move  zero           to   w-txe-sti
           else      move  1              to   w-txe-sti              .
           add       v-pos
                     v-car              giving w-txe-eni              .
           if        w-txe-eni            >    z-mmx
                     move  zero           to   w-txe-eni
           else      add   1
                           w-txe-sti
                           v-car        giving w-txe-eni              .
      *              *-------------------------------------------------*
      *              * Determinazione dell'ampiezza totale del campo   *
      *              * espanso compresi i delimitatori                 *
      *              *-------------------------------------------------*
           move      w-txe-enp            to   w-txe-siz              .
           subtract  w-txe-stp            from w-txe-siz              .
           add       1                    to   w-txe-siz              .
       sav-txe-999.
           exit.

      *================================================================*
      * Espansione campo tipo 'text' o 'espanso'                       *
      *----------------------------------------------------------------*
       exp-txe-000.
      *              *-------------------------------------------------*
      *              * Se il campo risulta gia' espanso non si esegue  *
      *              * alcuna azione                                   *
      *              *-------------------------------------------------*
           if        w-txe-exp            not  = spaces
                     go to exp-txe-999.
      *              *-------------------------------------------------*
      *              * Segnale di campo espanso                        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-txe-exp              .
      *              *-------------------------------------------------*
      *              * Preparazione parametri                          *
      *              *-------------------------------------------------*
           move      w-txe-stl            to   w-lin                  .
           move      w-txe-stp            to   w-pos                  .
           move      w-txe-siz            to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Linea di delimitazione superiore                *
      *              *-------------------------------------------------*
           move      w-alm                to   w-fld                  .
           if        w-txe-sti            not  = zero
                     move  "+"            to   w-chr (w-txe-sti)      .
           if        w-txe-eni            not  = zero
                     move  "+"            to   w-chr (w-txe-eni)      .
           perform   vis-mef-000          thru vis-mef-999            .
           add       1                    to   w-lin                  .
      *              *-------------------------------------------------*
      *              * Linee del campo espanso vero e proprio          *
      *              *-------------------------------------------------*
           move      1                    to   w-txe-wnl
                                               w-txe-wpu              .
           move      spaces               to   w-txe-wex              .
           if        w-txe-sti            not  = zero
                     move  "|"            to   w-txe-we1              .
       exp-txe-200.
           move      w-txe-wpu            to   w-pnt                  .
           if        w-txe-sti            not  = zero
                     go to exp-txe-400.
           unstring  v-txt                into w-txe-wex
                                  with pointer w-pnt                  .
           go to     exp-txe-600.
       exp-txe-400.
           unstring  v-txt                into w-txe-we2
                                  with pointer w-pnt                  .
       exp-txe-600.
           move      w-txe-wex            to   w-fld                  .
           if        w-txe-eni            not  = zero
                     move  "|"            to   w-chr (w-txe-eni)      .
           perform   vis-mef-000          thru vis-mef-999            .
           add       1                    to   w-lin                  .
           if        w-txe-wnl            <    v-ldt
                     add   1              to   w-txe-wnl
                     add   v-car          to   w-txe-wpu
                     go to exp-txe-200.
      *              *-------------------------------------------------*
      *              * Linea di delimitazione inferiore                *
      *              *-------------------------------------------------*
           move      w-alm                to   w-fld                  .
           if        w-txe-sti            not  = zero
                     move  "+"            to   w-chr (w-txe-sti)      .
           if        w-txe-eni            not  = zero
                     move  "+"            to   w-chr (w-txe-eni)      .
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Box grafico eventuale                           *
      *              *-------------------------------------------------*
           move      w-txe-stl            to   w-box-lin              .
           move      w-txe-stp            to   w-box-pos              .
      *
           move      w-txe-stl            to   w-box-lto              .
           add       v-ldt                to   w-box-lto              .
           add       1                    to   w-box-lto              .
      *
           move      w-txe-stp            to   w-box-pto              .
           add       w-txe-siz            to   w-box-pto              .
           subtract  1                    from w-box-pto              .
           perform   bxg-000              thru bxg-999                .
       exp-txe-999.
           exit.

      *================================================================*
      * Compressione area video per campo 'text' o 'espanso'           *
      *----------------------------------------------------------------*
       cmp-txe-000.
      *              *-------------------------------------------------*
      *              * Se il campo non risulta espanso non si esegue   *
      *              * la compressione                                 *
      *              *-------------------------------------------------*
           if        w-txe-exp            =    spaces
                     go to cmp-txe-700.
      *              *-------------------------------------------------*
      *              * Segnale di campo non espanso                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-txe-exp              .
      *              *-------------------------------------------------*
      *              * Preparazione parametri                          *
      *              *-------------------------------------------------*
           move      w-txe-stl            to   w-lin                  .
           move      w-txe-stp            to   w-pos                  .
           move      w-txe-siz            to   w-siz                  .
           move      spaces               to   w-fld                  .
       cmp-txe-100.
      *              *-------------------------------------------------*
      *              * Ripristino linee                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se fine linee                          *
      *                  *---------------------------------------------*
           if        w-lin                >    w-txe-enl
                     go to cmp-txe-999.
      *                  *---------------------------------------------*
      *                  * Test su tipo linea                          *
      *                  *---------------------------------------------*
           if        w-lin                =    v-lin
                     go to cmp-txe-300.
      *                  *---------------------------------------------*
      *                  * Se linea generica da ripristinare           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Estrazione linea da buffer video        *
      *                      *-----------------------------------------*
           move      w-pos                to   w-pnt                  .
           unstring  w-txe-siv-lin (w-lin)
                                          into w-fld
                                  with pointer w-pnt                  .
       cmp-txe-200.
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
           add       1                    to   w-lin                  .
           go to     cmp-txe-100.
       cmp-txe-300.
      *                  *---------------------------------------------*
      *                  * Se linea di accettazione                    *
      *                  *---------------------------------------------*
           perform   edt-000              thru edt-999                .
           move      v-edt                to   w-svf                  .
           if        w-txe-tdc            =    "U"
                     perform  und-txe-000 thru und-txe-999
           else      perform  def-txe-000 thru def-txe-999            .
           go to     cmp-txe-200.
       cmp-txe-700.
      *              *-------------------------------------------------*
      *              * Se campo non espanso                            *
      *              *-------------------------------------------------*
           perform   edt-000              thru edt-999                .
           move      v-edt                to   w-svf                  .
           if        w-txe-tdc            =    "U"
                     perform  und-txe-000 thru und-txe-999
           else      perform  def-txe-000 thru def-txe-999            .
       cmp-txe-999.
           exit.

      *----------------------------------------------------------------*
      *    Visualizzazione default con underscores per campo 'text' o  *
      *    'espanso' del valore contenuto in w-svf                     *
      *----------------------------------------------------------------*
       und-txe-000.
           move      v-lin                to   w-lin                  .
           move      w-txe-stp            to   w-pos                  .
           move      w-txe-siz            to   w-siz                  .
           move      w-svf                to   w-txe-we2              .
           inspect   w-txe-we2       replacing all spaces by w-unu    .
           if        w-txe-sti            not  = zero
                     if    v-tip          =    "T"
                           move  "|"      to   w-txe-we1
                     else  move  "["      to   w-txe-we1              .
           if        w-txe-eni            not  = zero
                     subtract  1          from w-txe-eni
                                        giving w-pnt
                     if    v-tip          =    "T"
                           move  "|"      to   w-txe-wec (w-pnt)
                     else  move  "]"      to   w-txe-wec (w-pnt)      .
           if        w-txe-sti            =    zero
                     move  w-txe-we2      to   w-fld
           else      move  w-txe-wex      to   w-fld                  .
           perform   vis-mef-000          thru vis-mef-999            .
       und-txe-999.
           exit.

      *----------------------------------------------------------------*
      *    Visualizzazione finale senza underscores per campo 'text' o *
      *    'espanso' del valore contenuto in w-svf                     *
      *----------------------------------------------------------------*
       def-txe-000.
           move      v-lin                to   w-lin                  .
           move      w-txe-stp            to   w-pos                  .
           move      w-txe-siz            to   w-siz                  .
           move      w-svf                to   w-txe-we2              .
           if        w-txe-sti            not  = zero
                     move  w-txe-siv-chr (w-lin,
                                          w-txe-stp)
                                          to   w-txe-we1              .
           if        w-txe-eni            not  = zero
                     subtract  1          from w-txe-eni
                                        giving w-pnt
                     move   w-txe-siv-chr (w-lin,
                                           w-txe-enp)
                                          to   w-txe-wec (w-pnt)      .
           if        w-txe-sti            =    zero
                     move  w-txe-we2      to   w-fld
           else      move  w-txe-wex      to   w-fld                  .
           perform   vis-mef-000          thru vis-mef-999            .
       def-txe-999.
           exit.

      *================================================================*
      *    Accept espanso                                              *
      *----------------------------------------------------------------*
       acc-esp-000.
      *              *-------------------------------------------------*
      *              * Tipo di accettazione campo espanso : normale    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cte                  .
      *              *-------------------------------------------------*
      *              * Accettazione campo di tipo espanso              *
      *              *-------------------------------------------------*
           perform   acc-cte-000          thru acc-cte-999            .
       acc-esp-999.
           exit.

      *================================================================*
      *    Accettazione campo di tipo espanso                          *
      *----------------------------------------------------------------*
       acc-cte-000.
      *              *-------------------------------------------------*
      *              * Salvataggio del valore alfabetico, nel caso si  *
      *              * trattasse di un check-box                       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-esp-alf              .
      *              *-------------------------------------------------*
      *              * Editing del campo espanso                       *
      *              *-------------------------------------------------*
           perform   edt-esp-000          thru edt-esp-999            .
           move      v-edt                to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Determinazione se compressione inibita, solo se *
      *              * non check-box, per valore attuale a zero e ele- *
      *              * mento manuale non ammesso. Per non consentire   *
      *              * la chiusura del campo espanso senza aver prima  *
      *              * selezionato un elemento                         *
      *              *-------------------------------------------------*
           if        w-cte                not  = "C"  and
                     w-txe-man            =    spaces and
                     v-num                =    zero
                     move  "#"            to   w-txe-nco              .
      *              *-------------------------------------------------*
      *              * Forzature in caso di check-box                  *
      *              *-------------------------------------------------*
           if        w-cte                =    "C"
                     move  "X"            to   w-txe-pee
                     move  spaces         to   w-txe-man              .
      *              *-------------------------------------------------*
      *              * Salvataggio valori di default per indice e va-  *
      *              * lore letterale                                  *
      *              *-------------------------------------------------*
           move      v-num                to   w-txe-svn              .
           move      v-alf                to   w-txe-sva              .
      *              *-------------------------------------------------*
      *              * Salvataggio area video potenzialmente occupata  *
      *              * dal campo in caso di espansione                 *
      *              *-------------------------------------------------*
           perform   sav-txe-000          thru sav-txe-999            .
      *              *-------------------------------------------------*
      *              * Se pre-espansione comunque richiesta : espan-   *
      *              * sione e navigazione                             *
      *              *-------------------------------------------------*
           if        w-txe-pee            not  = spaces
                     go to acc-cte-200.
      *              *-------------------------------------------------*
      *              * Se valore manuale non ammesso ed indice di de-  *
      *              * fault a zero : espansione e navigazione         *
      *              *-------------------------------------------------*
           if        w-txe-man            =    spaces and
                     w-txe-svn            =    zero
                     go to acc-cte-200.
      *              *-------------------------------------------------*
      *              * Se valore manuale ammesso ed indice di default  *
      *              * a zero e valore attuale a spaces: espansione e  *
      *              * navigazione                                     *
      *              *-------------------------------------------------*
           if        w-txe-man            not  = spaces and
                     w-txe-svn            =    zero     and
                     w-txe-sva            =    spaces
                     go to acc-cte-200.
       acc-cte-050.
      *              *-------------------------------------------------*
      *              * Se valore manuale ammesso si va' all'imposta-   *
      *              * zione del valore manuale                        *
      *              *-------------------------------------------------*
           if        w-txe-man            not  = spaces
                     go to acc-cte-660.
       acc-cte-100.
      *              *-------------------------------------------------*
      *              * Accettazione per valore manuale non ammesso     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore di default in work-area              *
      *                  *---------------------------------------------*
           move      v-alf                to   w-fld                  .
      *                  *---------------------------------------------*
      *                  * Inizializzazione indicatore carattere       *
      *                  *---------------------------------------------*
           move      1                    to   w-ccr                  .
       acc-cte-110.
      *                  *---------------------------------------------*
      *                  * Visualizzazione default con underscores     *
      *                  *---------------------------------------------*
           move      w-fld                to   w-svf                  .
           perform   und-txe-000          thru und-txe-999            .
           move      w-svf                to   w-fld                  .
           add       w-ccr                to   w-pos                  .
           subtract  1                    from w-pos                  .
      *                  *---------------------------------------------*
      *                  * Preparazione parametri in work-area         *
      *                  *---------------------------------------------*
           move      v-car                to   w-siz                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
       acc-cte-120.
      *                  *---------------------------------------------*
      *                  * Salvataggio carattere precedente            *
      *                  *---------------------------------------------*
           move      w-chr(w-ccr)         to   w-sav                  .
      *                  *---------------------------------------------*
      *                  * Impostazione carattere attuale e determina- *
      *                  * zione del tasto di terminazione usato       *
      *                  *---------------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
      *                  *---------------------------------------------*
      *                  * Se impostato un carattere                   *
      *                  *---------------------------------------------*
           if        k-key                not  = spaces
                     go to acc-cte-130.
      *                      *-----------------------------------------*
      *                      * Test se carattere accettabile           *
      *                      *                                         *
      *                      * N.B. : Modifica per accettazione di     *
      *                      *        caratteri nei campi espansi      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non prevista accettazione di ca- *
      *                          * ratteri : a ripristino carattere    *
      *                          *-------------------------------------*
           if        w-txe-mef            =    spaces
                     go to acc-cte-125.
      *                          *-------------------------------------*
      *                          * Preparazione scansione lista di ca- *
      *                          * ratteri per vedere se quello accet- *
      *                          * tato e' previsto nella lista        *
      *                          *-------------------------------------*
           move      w-ccr                to   w-txe-mes              .
           move      w-chr(w-ccr)         to   w-txe-mec              .
           move      w-txe-mec            to   w-fld                  .
           perform   upp-cas-000          thru upp-cas-999            .
           move      w-fld                to   w-txe-mec              .
           move      spaces               to   w-txe-met              .
           move      zero                 to   w-txe-mey              .
           move      w-txe-mes            to   w-ccr                  .
      *                          *-------------------------------------*
      *                          * Il carattere spaces non passa       *
      *                          *-------------------------------------*
           if        w-txe-mec            =    spaces
                     go to acc-cte-125.
      *                          *-------------------------------------*
      *                          * Scansione lista di caratteri        *
      *                          *-------------------------------------*
           set       w-txe-mei            to   01                     .
           search    w-txe-mer
                     when   w-txe-mer
                           (w-txe-mei)    =    w-txe-mec
                            move  "#"     to   w-txe-met
                            move  w-txe-mei
                                          to   w-txe-mey              .
      *                          *-------------------------------------*
      *                          * Se esito della ricerca negativo : a *
      *                          * ripristino carattere                *
      *                          *-------------------------------------*
           if        w-txe-met            =    spaces
                     go to acc-cte-125.
      *                          *-------------------------------------*
      *                          * Indice trovato in valore di desti-  *
      *                          * nazione                             *
      *                          *-------------------------------------*
           if        w-txe-mey            >    w-txe-mej
                     subtract w-txe-mej   from w-txe-mey              .
           move      w-txe-mey            to   v-num                  .
      *                          *-------------------------------------*
      *                          * Forzatura terminatore               *
      *                          *-------------------------------------*
           move      "RTRN"               to   k-key                  .
      *                          *-------------------------------------*
      *                          * Ad impostazione terminatore         *
      *                          *-------------------------------------*
           go to     acc-cte-130.
       acc-cte-125.
      *                      *-----------------------------------------*
      *                      * Ripristino carattere salvato            *
      *                      *-----------------------------------------*
           move      w-sav                to   w-chr (w-ccr)          .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione del carattere prece-  *
      *                      * dentemente salvato                      *
      *                      *-----------------------------------------*
           perform   vis-mec-000          thru vis-mec-999            .
      *                      *-----------------------------------------*
      *                      * Il carattere non viene accettato        *
      *                      *-----------------------------------------*
           go to     acc-cte-120.
       acc-cte-130.
      *                  *---------------------------------------------*
      *                  * Se impostato un terminatore                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ripristino carattere salvato            *
      *                      *-----------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione del carattere prece-  *
      *                      * dentemente salvato                      *
      *                      *-----------------------------------------*
           perform   vis-mec-000          thru vis-mec-999            .
       acc-cte-140.
      *                      *-----------------------------------------*
      *                      * Se function key Find                    *
      *                      *                 Expd                    *
      *                      *-----------------------------------------*
           if        k-key                =    "FIND" or
                     k-key                =    "EXPD"
                     go to acc-cte-200.
      *                      *-----------------------------------------*
      *                      * Se function key Rtrn                    *
      *                      *-----------------------------------------*
           if        k-key                =    "RTRN"
                     move  spaces         to   v-key
                     go to acc-cte-900.
      *                      *-----------------------------------------*
      *                      * Se function key Rght                    *
      *                      *                 Left                    *
      *                      *                 Apnd                    *
      *                      *                 Copy                    *
      *                      *                 Past                    *
      *                      *-----------------------------------------*
           if        k-key                =    "RGHT" or
                     k-key                =    "LEFT" or
                     k-key                =    "APND" or
                     k-key                =    "COPY" or
                     k-key                =    "PAST"
                     go to acc-cte-120.
      *                      *-----------------------------------------*
      *                      * Se altra function key                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se function-key prevista       *
      *                          *-------------------------------------*
           perform   tst-pfk-000          thru tst-pfk-999            .
      *                          *-------------------------------------*
      *                          * Se function-key non prevista        *
      *                          *-------------------------------------*
           if        k-key                =    "ICHR" or
                     k-key                =    "DCHR"
                     move  high-value     to   k-key                  .
           if        k-key                =    high-value
                     go to acc-cte-120.
       acc-cte-150.
      *                          *-------------------------------------*
      *                          * Se altra function key prevista      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Function key in uscita          *
      *                              *---------------------------------*
           move      k-key                to   v-key                  .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     acc-cte-900.
       acc-cte-200.
      *              *-------------------------------------------------*
      *              * Espansione del campo                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine di espansione                    *
      *                  *---------------------------------------------*
           perform   exp-txe-000          thru exp-txe-999            .
      *                  *---------------------------------------------*
      *                  * Navigazione su espansione                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice                 *
      *                      *-----------------------------------------*
           move      v-num                to   w-txe-inx              .
           if        w-txe-inx            =    zero
                     move  1              to   w-txe-inx              .
       acc-cte-210.
      *                      *-----------------------------------------*
      *                      * Accettazione di un carattere alla linea *
      *                      * corrispondente all'indice               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione parametri in work-area *
      *                          *-------------------------------------*
           move      v-car                to   w-siz                  .
           move      w-txe-stl            to   w-lin                  .
           add       w-txe-inx            to   w-lin                  .
           move      v-pos                to   w-pos                  .
           if        w-cte                =    "C" and
                     w-pos                <    80
                     add   1              to   w-pos                  .
      *                          *-------------------------------------*
      *                          * Estrazione in w-fld del valore del- *
      *                          * l'elemento da v-txt                 *
      *                          *-------------------------------------*
           move      w-txe-inx            to   w-pnt                  .
           subtract  1                    from w-pnt                  .
           multiply  v-car                by   w-pnt                  .
           add       1                    to   w-pnt                  .
           move      spaces               to   w-fld                  .
           unstring  v-txt                into w-fld
                                  with pointer w-pnt                  .
           move      v-car                to   w-pnt                  .
           add       1                    to   w-pnt                  .
           move      high-value           to   w-chr (w-pnt)          .
           inspect   w-fld           replacing characters
                                          by   spaces
                                         after initial high-value     .
           move      spaces               to   w-chr (w-pnt)          .
      *                          *-------------------------------------*
      *                          * Indicatore carattere                *
      *                          *-------------------------------------*
           if        w-cte                =    "C"
                     move  2              to   w-ccr
           else      move  1              to   w-ccr                  .
       acc-cte-220.
      *                          *-------------------------------------*
      *                          * Salvataggio 1. o 2. carattere       *
      *                          *-------------------------------------*
           if        w-cte                =    "C"
                     move  w-chr (2)      to   w-sav
           else      move  w-chr (1)      to   w-sav                  .
      *                          *-------------------------------------*
      *                          * Impostazione 1. o 2. carattere e    *
      *                          * determinazione del tasto di termi-  *
      *                          * nazione usato                       *
      *                          *-------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
       acc-cte-222.
      *                          *-------------------------------------*
      *                          * Se impostato un carattere           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test                            *
      *                              *---------------------------------*
           if        k-key                not  = spaces
                     go to acc-cte-230.
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cte                =    "C"
                     go to acc-cte-226.
       acc-cte-224.
      *                              *---------------------------------*
      *                              * Se impostazione normale         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Test se carattere accetta-  *
      *                                  * bile                        *
      *                                  *                             *
      *                                  * N.B. : Modifica per accet-  *
      *                                  *        tazione di caratteri *
      *                                  *        nei campi espansi    *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Se non prevista accet-  *
      *                                      * tazione di caratteri :  *
      *                                      * a ripristino carattere  *
      *                                      *-------------------------*
           if        w-txe-mef            =    spaces
                     go to acc-cte-225.
      *                                      *-------------------------*
      *                                      * Preparazione scansione  *
      *                                      * lista di caratteri per  *
      *                                      * vedere se quello accet- *
      *                                      * tato e' previsto nella  *
      *                                      * lista                   *
      *                                      *-------------------------*
           move      w-ccr                to   w-txe-mes              .
           move      w-chr(w-ccr)         to   w-txe-mec              .
           move      w-txe-mec            to   w-fld                  .
           perform   upp-cas-000          thru upp-cas-999            .
           move      w-fld                to   w-txe-mec              .
           move      spaces               to   w-txe-met              .
           move      zero                 to   w-txe-mey              .
           move      w-txe-mes            to   w-ccr                  .
      *                                      *-------------------------*
      *                                      * Scansione lista di ca-  *
      *                                      * ratteri                 *
      *                                      *-------------------------*
           set       w-txe-mei            to   01                     .
           search    w-txe-mer
                     when   w-txe-mer
                           (w-txe-mei)    =    w-txe-mec
                            move  "#"     to   w-txe-met
                            move  w-txe-mei
                                          to   w-txe-mey              .
      *                                      *-------------------------*
      *                                      * Se esito della ricerca  *
      *                                      * negativo : a ripristino *
      *                                      * carattere               *
      *                                      *-------------------------*
           if        w-txe-met            =    spaces
                     go to acc-cte-225.
      *                                      *-------------------------*
      *                                      * Indice trovato in valo- *
      *                                      * re di destinazione      *
      *                                      *-------------------------*
           if        w-txe-mey            >    w-txe-mej
                     subtract w-txe-mej   from w-txe-mey              .
           move      w-txe-mey            to   w-txe-inx              .
      *                                      *-------------------------*
      *                                      * Estrazione in w-fld del *
      *                                      * valore dell'elemento da *
      *                                      * v-txt                   *
      *                                      *-------------------------*
           move      w-txe-inx            to   w-pnt                  .
           subtract  1                    from w-pnt                  .
           multiply  v-car                by   w-pnt                  .
           add       1                    to   w-pnt                  .
           move      spaces               to   w-fld                  .
           unstring  v-txt                into w-fld
                                  with pointer w-pnt                  .
           move      v-car                to   w-pnt                  .
           add       1                    to   w-pnt                  .
           move      high-value           to   w-chr (w-pnt)          .
           inspect   w-fld           replacing characters
                                          by   spaces
                                         after initial high-value     .
           move      spaces               to   w-chr (w-pnt)          .
      *                                      *-------------------------*
      *                                      * Forzatura terminatore   *
      *                                      *-------------------------*
           move      "RTRN"               to   k-key                  .
      *                                      *-------------------------*
      *                                      * Ad impostazione termi-  *
      *                                      * natori                  *
      *                                      *-------------------------*
           go to     acc-cte-230.
       acc-cte-225.
      *                                  *-----------------------------*
      *                                  * Ripristino carattere salva- *
      *                                  * to                          *
      *                                  *-----------------------------*
           move      w-sav                to   w-chr (1)              .
      *                                  *-----------------------------*
      *                                  * Rivisualizzazione del ca-   *
      *                                  * rattere precedentemente     *
      *                                  * salvato                     *
      *                                  *-----------------------------*
           perform   vis-mec-000          thru vis-mec-999            .
      *                                  *-----------------------------*
      *                                  * Il carattere viene rifiuta- *
      *                                  * to                          *
      *                                  *-----------------------------*
           go to     acc-cte-220.
       acc-cte-226.
      *                              *---------------------------------*
      *                              * Se impostazione di check-box    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Normalizzazione del carat-  *
      *                                  * tere impostato              *
      *                                  *-----------------------------*
           if        w-chr (2)            not  = spaces
                     move  "X"            to   w-chr (2)              .
      *                                  *-----------------------------*
      *                                  * Memorizzazione del caratte- *
      *                                  * re impostato nel valore al- *
      *                                  * fabetico                    *
      *                                  *-----------------------------*
           move      w-chr (2)            to   w-esp-alf
                                              (w-txe-inx : 1)         .
      *                                  *-----------------------------*
      *                                  * Rivisualizzazione del ca-   *
      *                                  * rattere impostato e norma-  *
      *                                  * lizzato                     *
      *                                  *-----------------------------*
           perform   vis-mec-000          thru vis-mec-999            .
      *                                  *-----------------------------*
      *                                  * Simulazione di Down         *
      *                                  *-----------------------------*
           move      "DOWN"               to   k-key                  .
           go to     acc-cte-245.
       acc-cte-230.
      *                          *-------------------------------------*
      *                          * Se impostato un terminatore         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Ripristino carattere salvato    *
      *                              *---------------------------------*
           if        w-cte                =    "C"
                     move  w-sav          to   w-chr (2)
           else      move  w-sav          to   w-chr (1)              .
       acc-cte-240.
      *                              *---------------------------------*
      *                              * Se function key Find            *
      *                              *                 Expd            *
      *                              *---------------------------------*
           if        k-key                =    "FIND" or
                     k-key                =    "EXPD"
                     go to acc-cte-300.
      *                              *---------------------------------*
      *                              * Se function key Down            *
      *                              *---------------------------------*
           if        k-key                not  = "DOWN"
                     go to acc-cte-250.
       acc-cte-245.
      *                                  *-----------------------------*
      *                                  * Se impostazione check-box e *
      *                                  * su ultima linea : a fine    *
      *                                  * impostazione                *
      *                                  *-----------------------------*
           if        w-cte                =    "C"   and
                     w-txe-inx            =    v-ldt
                     go to acc-cte-270.
      *                                  *-----------------------------*
      *                                  * Incremento indice           *
      *                                  *-----------------------------*
           if        w-txe-inx            <    v-ldt
                     add   1              to   w-txe-inx              .
      *                                  *-----------------------------*
      *                                  * Riciclo su linea            *
      *                                  *-----------------------------*
           go to     acc-cte-210.
       acc-cte-250.
      *                              *---------------------------------*
      *                              * Se function key Up              *
      *                              *---------------------------------*
           if        k-key                not  = "UP  "
                     go to acc-cte-260.
      *                                  *-----------------------------*
      *                                  * Se indice maggiore di 1, si *
      *                                  * decrementa l'indice e si    *
      *                                  * ricicla sulla linea, al-    *
      *                                  * trimenti : a fine imposta-  *
      *                                  * zione                       *
      *                                  *-----------------------------*
           if        w-txe-inx            >    1
                     subtract 1           from w-txe-inx
                     go to    acc-cte-210
           else      go to    acc-cte-270.
       acc-cte-260.
      *                              *---------------------------------*
      *                              * Se function key Slct            *
      *                              *---------------------------------*
           if        k-key                not  = "SLCT"
                     go to acc-cte-270.
      *                                  *-----------------------------*
      *                                  * Se impostazione normale :   *
      *                                  * continuazione               *
      *                                  *-----------------------------*
           if        w-cte                not  = "C"
                     go to acc-cte-270.
      *                                  *-----------------------------*
      *                                  * Inversione carattere impo-  *
      *                                  * stato                       *
      *                                  *-----------------------------*
           if        w-sav                =    spaces
                     move  "X"            to   w-chr (2)
           else      move  spaces         to   w-chr (2)              .
      *                                  *-----------------------------*
      *                                  * Riaggancio come se imposta- *
      *                                  * zione carattere             *
      *                                  *-----------------------------*
           go to     acc-cte-226.
       acc-cte-270.
      *                              *---------------------------------*
      *                              * Se function key Slct            *
      *                              *                 Rtrn            *
      *                              *                 Do              *
      *                              *                 Delt            *
      *                              *                 Prsc            *
      *                              *                 Nxsc            *
      *                              *                 Up              *
      *                              *                 Find            *
      *                              *                 Expd            *
      *                              *                 Down            *
      *                              *                 Exit            *
      *                              *---------------------------------*
           if        k-key                =    "SLCT" or
                     k-key                =    "RTRN" or
                     k-key                =    "FIND" or
                     k-key                =    "EXPD"
                     go to acc-cte-275.
           if        k-key                not  = "DO  " and
                     k-key                not  = "DELT" and
                     k-key                not  = "EXIT" and
                     k-key                not  = "PRSC" and
                     k-key                not  = "NXSC" and
                     k-key                not  = "UP  " and
                     k-key                not  = "DOWN"
                     go to acc-cte-220.
       acc-cte-271.
           move      zero                 to   w-pnt                  .
       acc-cte-272.
           if        w-pnt                <    40
                     add   1              to   w-pnt
                     if    v-pfk(w-pnt)   =    "DO  " or
                           v-pfk(w-pnt)   =    "DELT" or
                           v-pfk(w-pnt)   =    "EXIT" or
                           v-pfk(w-pnt)   =    "PRSC" or
                           v-pfk(w-pnt)   =    "NXSC" or
                           v-pfk(w-pnt)   =    "UP  " or
                           v-pfk(w-pnt)   =    "DOWN"
                           go to acc-cte-275
                     else  go to acc-cte-272.
           go to     acc-cte-220.
       acc-cte-275.
      *                                  *-----------------------------*
      *                                  * Preparazione indice         *
      *                                  *-----------------------------*
           move      w-txe-inx            to   v-num                  .
      *                                  *-----------------------------*
      *                                  * Preparazione valore         *
      *                                  *-----------------------------*
           move      w-fld                to   v-alf                  .
       acc-cte-277.
      *                                  *-----------------------------*
      *                                  * Se accettazione di tipo     *
      *                                  * check-box si modifica il    *
      *                                  * valore in uscita ed anche   *
      *                                  * valore di v-txt in modo     *
      *                                  * tale che la routine di com- *
      *                                  * pressione visualizzi il     *
      *                                  * valore alfanumerico         *
      *                                  *-----------------------------*
           if        w-cte                not  = "C"
                     go to acc-cte-279.
      *                                      *-------------------------*
      *                                      * Editing check-box       *
      *                                      *-------------------------*
           move      w-ckb-ldt            to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-esp-alf            to   v-alf                  .
           perform   edt-ckb-000          thru edt-ckb-999            .
      *                                      *-------------------------*
      *                                      * Modifica di v-txt       *
      *                                      *-------------------------*
           move      w-txe-inx            to   w-inx                  .
           subtract  1                    from w-inx                  .
           multiply  v-car                by   w-inx                  .
           add       1                    to   w-inx                  .
           move      all   spaces         to   v-txt
                                              (w-inx : v-car)         .
           move      v-edt
                    (01 : v-edl)          to   v-txt
                                              (w-inx : v-edl)         .
      *                                      *-------------------------*
      *                                      * Modifica di v-alf       *
      *                                      *-------------------------*
           move      w-esp-alf            to   v-alf                  .
       acc-cte-279.
      *                                  *-----------------------------*
      *                                  * Subroutine di compressione  *
      *                                  *-----------------------------*
           move      "D"                  to    w-txe-tdc             .
           perform   cmp-txe-000          thru cmp-txe-999            .
      *                                  *-----------------------------*
      *                                  * Function key al suo valore  *
      *                                  *-----------------------------*
           if        k-key                =    "DO  "
                     move  "DO  "         to   v-key
           else if   k-key                =    "DELT"
                     move  "DELT"         to   v-key
           else if   k-key                =    "EXIT"
                     move  "EXIT"         to   v-key
           else if   k-key                =    "PRSC"
                     move  "PRSC"         to   v-key
           else if   k-key                =    "NXSC"
                     move  "NXSC"         to   v-key
           else if   k-key                =    "UP  "
                     move  "UP  "         to   v-key
           else if   k-key                =    "DOWN"
                     move  "DOWN"         to   v-key
           else if   k-key                =    "FIND"
                     move  "FIND"         to   v-key
           else if   k-key                =    "EXPD"
                     move  "EXPD"         to   v-key
           else      move  spaces         to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     acc-cte-950.
       acc-cte-300.
      *              *-------------------------------------------------*
      *              * Compressione del campo                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se in impostazione check-box : riaggancio   *
      *                  * per uscire                                  *
      *                  *---------------------------------------------*
           if        w-cte                =    "C"
                     go to acc-cte-270.
      *                  *---------------------------------------------*
      *                  * Se compressione inibita, ma pre-espansione  *
      *                  * forzata : come Slct o Return                *
      *                  *---------------------------------------------*
           if        w-txe-pee            =    "X" and
                     w-txe-nco            =    "#"
                     move  "SLCT"         to   k-key
                     go to acc-cte-270.
      *                  *---------------------------------------------*
      *                  * Se compressione inibita : reimpostazione    *
      *                  *---------------------------------------------*
           if        w-txe-nco            =    "#"
                     go to acc-cte-210.
      *                  *---------------------------------------------*
      *                  * Subroutine di compressione                  *
      *                  *---------------------------------------------*
           move      "U"                  to   w-txe-tdc              .
           perform   cmp-txe-000          thru cmp-txe-999            .
      *                  *---------------------------------------------*
      *                  * Ad accettazione manuale o no                *
      *                  *---------------------------------------------*
           go to     acc-cte-050.
       acc-cte-660.
      *              *-------------------------------------------------*
      *              * Accettazione eventuale valore manuale           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore di default in work-area              *
      *                  *---------------------------------------------*
           move      v-alf                to   w-fld                  .
      *                  *---------------------------------------------*
      *                  * Inizializz. indicatore carattere attuale    *
      *                  *---------------------------------------------*
           move      1                    to   w-ccr                  .
      *                  *---------------------------------------------*
      *                  * Segnale prima impostazione                  *
      *                  *---------------------------------------------*
           move      "*"                  to   w-frs                  .
      *                  *---------------------------------------------*
      *                  * Segnale almeno una variazione : no          *
      *                  *---------------------------------------------*
           move      spaces               to   w-auv                  .
       acc-cte-670.
      *                  *---------------------------------------------*
      *                  * Visualizzazione default con underscores     *
      *                  *---------------------------------------------*
           move      w-fld                to   w-svf                  .
           perform   und-txe-000          thru und-txe-999            .
           move      w-svf                to   w-fld                  .
      *                  *---------------------------------------------*
      *                  * Preparazione parametri in work-area         *
      *                  *---------------------------------------------*
           move      v-car                to   w-siz                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           add       w-ccr                to   w-pos                  .
           subtract  1                    from w-pos                  .
       acc-cte-680.
      *                  *---------------------------------------------*
      *                  * Salvataggio carattere precedente            *
      *                  *---------------------------------------------*
           move      w-chr(w-ccr)         to   w-sav                  .
      *                  *---------------------------------------------*
      *                  * Impostazione carattere attuale e determina- *
      *                  * zione del tasto di terminazione usato       *
      *                  *---------------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
      *                  *---------------------------------------------*
      *                  * Se impostato un carattere                   *
      *                  *---------------------------------------------*
           if        k-key                not  = spaces
                     go to acc-cte-700.
      *                      *-----------------------------------------*
      *                      * Segnale almeno una variazione           *
      *                      *-----------------------------------------*
           if        w-chr(w-ccr)         not  = w-sav
                     move  "*"            to   w-auv                  .
      *                      *-----------------------------------------*
      *                      * Visualizzazione carattere impostato     *
      *                      *-----------------------------------------*
           perform   vis-mec-000          thru vis-mec-999            .
      *                      *-----------------------------------------*
      *                      * Se primo carattere impostato            *
      *                      *-----------------------------------------*
           if not   (w-frs                not  = spaces and
                     w-ccr                =    1          )
                     go to acc-cte-690.
      *                          *-------------------------------------*
      *                          * Segnale di non piu' prima digita-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      spaces               to   w-frs                  .
      *                          *-------------------------------------*
      *                          * Se il campo non e' variato si avan- *
      *                          * za alla prossima posizione          *
      *                          *-------------------------------------*
           if        w-fld                =    w-chr (1)
                     go to acc-cte-690.
      *                          *-------------------------------------*
      *                          * Altrimenti si reinizializza il cam- *
      *                          * po al valore del carattere e si va' *
      *                          * ad impostare i caratteri successivi *
      *                          *-------------------------------------*
           move      w-chr (1)            to   w-fld                  .
           move      2                    to   w-ccr                  .
           go to     acc-cte-670.
       acc-cte-690.
      *                          *-------------------------------------*
      *                          * Avanzamento alla prossima posizione *
      *                          * a meno di non essere gia' all'ulti- *
      *                          * ma posizione                        *
      *                          *-------------------------------------*
           if        w-ccr                not  = w-siz
                     add   1              to   w-ccr
                     add   1              to   w-pos                  .
           go to     acc-cte-680.
       acc-cte-700.
      *                  *---------------------------------------------*
      *                  * Se impostato un terminatore                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale non piu' prima digitazione      *
      *                      *-----------------------------------------*
           move      spaces               to   w-frs                  .
      *                      *-----------------------------------------*
      *                      * Ripristino carattere salvato            *
      *                      *-----------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
      *                      *-----------------------------------------*
      *                      * Se almeno una variazione si sposta il   *
      *                      * valore in uscita e si pone l'indice a   *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           if        w-auv                not  = spaces
                     move  w-fld          to   v-alf
                     move  zero           to   v-num                  .
       acc-cte-710.
      *                      *-----------------------------------------*
      *                      * Se function key Find                    *
      *                      *                 Expd                    *
      *                      *-----------------------------------------*
           if        k-key                =    "FIND" or
                     k-key                =    "EXPD"
                     go to acc-cte-200.
      *                      *-----------------------------------------*
      *                      * Se function key Rght                    *
      *                      *-----------------------------------------*
           if        k-key                =    "RGHT"
                     go to acc-cte-690.
      *                      *-----------------------------------------*
      *                      * Se function key Return                  *
      *                      *-----------------------------------------*
           if        k-key                =    "RTRN"
                     move  spaces         to   v-key
                     go to acc-cte-900.
      *                      *-----------------------------------------*
      *                      * Se function key Left                    *
      *                      *-----------------------------------------*
           if        k-key                not  = "LEFT"
                     go to acc-cte-750.
      *                          *-------------------------------------*
      *                          * Se si e' in una posizione superiore *
      *                          * alla prima si arretra               *
      *                          *-------------------------------------*
           if        w-ccr                not  = 1
                     subtract  1          from w-ccr
                                               w-pos
                     go to     acc-cte-680.
      *                          *-------------------------------------*
      *                          * Altrimenti se il contenuto attuale  *
      *                          * e' pari al valore di default lo si  *
      *                          * annulla, in caso contrario si ripri-*
      *                          * stina il valore di default          *
      *                          *-------------------------------------*
           if        w-txe-svn            =    zero
                     go to acc-cte-730
           else      go to acc-cte-740.
       acc-cte-730.
           if        v-num                =    zero
                     if     v-alf         =    w-txe-sva
                            move  spaces  to   v-alf
                     else   move  w-txe-sva
                                          to   v-alf
           else      move   zero          to   v-num
                     move   spaces        to   v-alf                  .
           go to     acc-cte-050.
       acc-cte-740.
           if        v-num                =    w-txe-svn
                     perform edt-esp-000  thru edt-esp-999
                     move    v-edt        to   v-alf
           else      move    zero         to   v-num
                     move    spaces       to   v-alf                  .
           go to     acc-cte-050.
       acc-cte-750.
      *                      *-----------------------------------------*
      *                      * Se function key Apnd                    *
      *                      *-----------------------------------------*
           if        k-key                not  = "APND"
                     go to acc-cte-780.
           move      v-car                to   w-ccr                  .
       acc-cte-760.
           if        w-chr (w-ccr)        =    spaces
                     if     w-ccr         >    1
                            subtract 1    from w-ccr
                            go to    acc-cte-760
                     else   go to    acc-cte-770.
           add       2                    to   w-ccr                  .
       acc-cte-770.
           if        w-ccr                >    v-car
                     move   v-car         to   w-ccr                  .
           add       v-pos
                     w-ccr              giving w-pos                  .
           subtract  1                    from w-pos                  .
           go to     acc-cte-680.
       acc-cte-780.
      *                      *-----------------------------------------*
      *                      * Se function key Copy                    *
      *                      *-----------------------------------------*
           if        k-key                =    "COPY"
                     move  v-alf          to   z-cla
                     go to acc-cte-680.
      *                      *-----------------------------------------*
      *                      * Se function key Past                    *
      *                      *-----------------------------------------*
           if        k-key                =    "PAST"
                     move  zero           to   v-num
                     move  z-cla          to   v-alf
                     go to acc-cte-050.
       acc-cte-790.
      *                      *-----------------------------------------*
      *                      * Se altra function key                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se function-key prevista       *
      *                          *-------------------------------------*
           perform   tst-pfk-000          thru tst-pfk-999            .
      *                          *-------------------------------------*
      *                          * Se function-key non prevista        *
      *                          *-------------------------------------*
           if        k-key                =    "ICHR" or
                     k-key                =    "DCHR"
                     move  high-value     to   k-key                  .
           if        k-key                =    high-value
                     go to acc-cte-680.
       acc-cte-800.
      *                          *-------------------------------------*
      *                          * Se altra function key prevista      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Function key in uscita          *
      *                              *---------------------------------*
           move      k-key                to   v-key                  .
       acc-cte-900.
      *              *-------------------------------------------------*
      *              * Uscita da impostazione                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione finale                      *
      *                  *---------------------------------------------*
           perform   edt-esp-000          thru edt-esp-999            .
           move      v-edt                to   w-svf                  .
           perform   def-txe-000          thru def-txe-999            .
       acc-cte-950.
      *                  *---------------------------------------------*
      *                  * Normalizzazione function keys               *
      *                  *---------------------------------------------*
           move      spaces               to   v-ufk                  .
      *                  *---------------------------------------------*
      *                  * Segnale se campo modificato                 *
      *                  *---------------------------------------------*
           if        v-num                =    zero
                     go to acc-cte-980
           else      go to acc-cte-990.
       acc-cte-980.
            if       w-txe-svn            =    zero and
                     w-txe-sva            =    v-alf
                     move  spaces         to   v-mod
             else    move  "#"            to   v-mod                  .
             go to   acc-cte-995.
       acc-cte-990.
             if      w-txe-svn            =    v-num
                     move  spaces         to   v-mod
             else    move  "#"            to   v-mod                  .
       acc-cte-995.
      *                  *---------------------------------------------*
      *                  * Se accettazione di check-box : si pone in   *
      *                  * uscita il valore alfanumerico               *
      *                  *---------------------------------------------*
           if        w-cte                =    "C"
                     move  w-esp-alf      to   v-alf                  .
       acc-cte-999.
           exit.

      *================================================================*
      *    Accept check-box                                            *
      *----------------------------------------------------------------*
       acc-ckb-000.
      *              *-------------------------------------------------*
      *              * Salvataggio valori in input                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      v-ope                to   w-ckb-ope              .
      *                  *---------------------------------------------*
      *                  * Tipo campo                                  *
      *                  *---------------------------------------------*
           move      v-tip                to   w-ckb-tip              .
      *                  *---------------------------------------------*
      *                  * Numero caratteri del literal                *
      *                  *---------------------------------------------*
           move      v-car                to   w-ckb-car              .
      *                  *---------------------------------------------*
      *                  * Numero linee di testo, ovvero elementi      *
      *                  *---------------------------------------------*
           move      v-ldt                to   w-ckb-ldt              .
      *                  *---------------------------------------------*
      *                  * Tipo editing                                *
      *                  *---------------------------------------------*
           move      v-edm                to   w-ckb-edm              .
      *                  *---------------------------------------------*
      *                  * Valore alfanumerico, ovvero serie di carat- *
      *                  * teri                                        *
      *                  *---------------------------------------------*
           move      v-alf                to   w-ckb-alf              .
      *                  *---------------------------------------------*
      *                  * Valore text, ovvero valore literals         *
      *                  *---------------------------------------------*
           move      v-txt                to   w-ckb-txt              .
      *                  *---------------------------------------------*
      *                  * Linea                                       *
      *                  *---------------------------------------------*
           move      v-lin                to   w-ckb-lin              .
      *                  *---------------------------------------------*
      *                  * Posizione                                   *
      *                  *---------------------------------------------*
           move      v-pos                to   w-ckb-pos              .
      *                  *---------------------------------------------*
      *                  * Lista delle function keys ammissibili       *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-ckb-ufk              .
       acc-ckb-025.
      *              *-------------------------------------------------*
      *              * Inizializzazione valore attuale serie di carat- *
      *              * teri                                            *
      *              *-------------------------------------------------*
           move      w-ckb-alf            to   w-ckb-vsr              .
       acc-ckb-050.
      *              *-------------------------------------------------*
      *              * Se la maschera di editing prevede il carattere  *
      *              * 'X' : ad accettazione espansa                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   w-ckb-edm        tallying w-ctr
                     for    all "X"                                   .
           if        w-ctr                >    zero
                     go to acc-ckb-200.
       acc-ckb-100.
      *              *-------------------------------------------------*
      *              * Accettazione campo chiuso                       *
      *              *-------------------------------------------------*
       acc-ckb-110.
      *                  *---------------------------------------------*
      *                  * Editing check-box                           *
      *                  *---------------------------------------------*
           move      w-ckb-ldt            to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-ckb-vsr            to   v-alf                  .
           perform   edt-ckb-000          thru edt-ckb-999            .
      *                  *---------------------------------------------*
      *                  * Preparazione accettazione alfanumerica      *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      v-edl                to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-ckb-lin            to   v-lin                  .
           move      w-ckb-pos            to   v-pos                  .
           move      v-edt                to   v-alf                  .
           move      w-ckb-ufk            to   v-ufk                  .
      *                  *---------------------------------------------*
      *                  * Tipo di accettazione alfanumerica : per un  *
      *                  * check-box                                   *
      *                  *---------------------------------------------*
           move      "C"                  to   w-cta                  .
      *                  *---------------------------------------------*
      *                  * Accettazione campo di tipo alfanumerico     *
      *                  *---------------------------------------------*
           perform   acc-cta-000          thru acc-cta-999            .
       acc-ckb-120.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale serie di carat-  *
      *                  * teri                                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-ckb-vsr              .
           move      zero                 to   w-ctr                  .
           move      zero                 to   w-inx                  .
       acc-ckb-122.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    w-ckb-ldt
                     go to acc-ckb-130.
           add       2                    to   w-inx                  .
           if        v-alf (w-inx : 01)   =    spaces
                     move  spaces         to   w-ckb-vsr (w-ctr : 01)
           else      move  "X"            to   w-ckb-vsr (w-ctr : 01) .
           go to     acc-ckb-122.
       acc-ckb-130.
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione valore editato            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing check-box                       *
      *                      *-----------------------------------------*
           move      w-ckb-ldt            to   v-ldt                  .
           move      w-ckb-edm            to   v-edm                  .
           move      w-ckb-vsr            to   v-alf                  .
           perform   edt-ckb-000          thru edt-ckb-999            .
      *                      *-----------------------------------------*
      *                      * Visualizzazione campo alfabetico        *
      *                      *-----------------------------------------*
           move      v-edt                to   w-fld                  .
           move      w-ckb-lin            to   v-lin                  .
           move      w-ckb-pos            to   v-pos
           move      v-edl                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
       acc-ckb-140.
      *                  *---------------------------------------------*
      *                  * Ripristino valori salvati, ad eccezione del *
      *                  * valore del campo impostato                  *
      *                  *---------------------------------------------*
           move      w-ckb-ope            to   v-ope                  .
           move      w-ckb-tip            to   v-tip                  .
           move      w-ckb-car            to   v-car                  .
           move      w-ckb-ldt            to   v-ldt                  .
           move      w-ckb-edm            to   v-edm                  .
           move      w-ckb-txt            to   v-txt                  .
           move      w-ckb-lin            to   v-lin                  .
           move      w-ckb-pos            to   v-pos                  .
           move      w-ckb-ufk            to   v-ufk                  .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di uscita     *
      *                  *---------------------------------------------*
           if        v-key                =    "FIND" or
                     v-key                =    "EXPD"
                     go to acc-ckb-200.
       acc-ckb-150.
      *                  *---------------------------------------------*
      *                  * Se uscita ne' con Find ne' con Expd         *
      *                  *---------------------------------------------*
      *                     *------------------------------------------*
      *                     * Segnale se campo modificato              *
      *                     *------------------------------------------*
           if        w-ckb-vsr            =    w-ckb-alf
                     move   spaces        to   v-mod
           else      move   "S"           to   v-mod                  .
      *                     *------------------------------------------*
      *                     * Normalizzazione f-keys possibili         *
      *                     *------------------------------------------*
           move      spaces               to   v-ufk                  .
      *                     *------------------------------------------*
      *                     * Valore in uscita                         *
      *                     *------------------------------------------*
           move      w-ckb-vsr            to   v-alf                  .
      *                     *------------------------------------------*
      *                     * Uscita                                   *
      *                     *------------------------------------------*
           go to     acc-ckb-999.
       acc-ckb-200.
      *              *-------------------------------------------------*
      *              * Accettazione espansa                            *
      *              *-------------------------------------------------*
       acc-ckb-300.
      *                  *---------------------------------------------*
      *                  * Preparazione accettazione campo espanso     *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-ckb-car            to   v-car                  .
           move      w-ckb-ldt            to   v-ldt                  .
           move      "X"                  to   v-edm                  .
           move      w-ckb-lin            to   v-lin                  .
           move      w-ckb-pos            to   v-pos                  .
           move      w-ckb-ufk            to   v-ufk                  .
           move      zero                 to   v-num                  .
           move      w-ckb-vsr            to   v-alf                  .
           move      w-ckb-txt            to   v-txt                  .
       acc-ckb-400.
      *                  *---------------------------------------------*
      *                  * Manipolazione del campo testo relativo alla *
      *                  * accettazione del campo espanso              *
      *                  *---------------------------------------------*
       acc-ckb-405.
           move      zero                 to   w-ctr                  .
       acc-ckb-410.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    w-ckb-ldt
                     go to acc-ckb-500.
           move      w-ctr                to   w-inx                  .
           subtract  1                    from w-inx                  .
           multiply  w-ckb-car            by   w-inx                  .
           add       2                    to   w-inx                  .
           if        w-ckb-vsr
                    (w-ctr : 1)           =    spaces
                     move  spaces         to   v-txt (w-inx : 1)
           else      move  "X"            to   v-txt (w-inx : 1)      .
           go to     acc-ckb-410.
       acc-ckb-500.
      *                  *---------------------------------------------*
      *                  * Tipo di accettazione campo espanso : per un *
      *                  * check-box                                   *
      *                  *---------------------------------------------*
           move      "C"                  to   w-cte                  .
      *                  *---------------------------------------------*
      *                  * Accettazione campo di tipo espanso          *
      *                  *---------------------------------------------*
           perform   acc-cte-000          thru acc-cte-999            .
       acc-ckb-600.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore attuale serie di carat-  *
      *                  * teri                                        *
      *                  *---------------------------------------------*
           move      v-alf                to   w-ckb-vsr              .
      *                  *---------------------------------------------*
      *                  * Visualizzazione campo in formato alfabetico *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing check-box                       *
      *                      *-----------------------------------------*
           move      w-ckb-ldt            to   v-ldt                  .
           move      w-ckb-edm            to   v-edm                  .
           move      w-ckb-vsr            to   v-alf                  .
           perform   edt-ckb-000          thru edt-ckb-999            .
      *                      *-----------------------------------------*
      *                      * Visualizzazione campo alfabetico        *
      *                      *-----------------------------------------*
           move      v-edt                to   w-fld                  .
           move      w-ckb-lin            to   w-lin                  .
           move      w-ckb-pos            to   w-pos
           move      v-edl                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *                  *---------------------------------------------*
      *                  * Ripristino valori salvati, ad eccezione del *
      *                  * valore del campo impostato                  *
      *                  *---------------------------------------------*
           move      w-ckb-ope            to   v-ope                  .
           move      w-ckb-tip            to   v-tip                  .
           move      w-ckb-car            to   v-car                  .
           move      w-ckb-ldt            to   v-ldt                  .
           move      w-ckb-edm            to   v-edm                  .
           move      w-ckb-txt            to   v-txt                  .
           move      w-ckb-lin            to   v-lin                  .
           move      w-ckb-pos            to   v-pos                  .
           move      w-ckb-ufk            to   v-ufk                  .
       acc-ckb-700.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di uscita     *
      *                  *---------------------------------------------*
           if        v-key                =    "FIND" or
                     v-key                =    "EXPD"
                     go to acc-ckb-100.
       acc-ckb-800.
      *                  *---------------------------------------------*
      *                  * Se uscita ne' con Find ne' con Expd         *
      *                  *---------------------------------------------*
      *                     *------------------------------------------*
      *                     * Segnale se campo modificato              *
      *                     *------------------------------------------*
           if        w-ckb-vsr            =    w-ckb-alf
                     move   spaces        to   v-mod
           else      move   "S"           to   v-mod                  .
      *                     *------------------------------------------*
      *                     * Normalizzazione f-keys possibili         *
      *                     *------------------------------------------*
           move      spaces               to   v-ufk                  .
      *                     *------------------------------------------*
      *                     * Uscita                                   *
      *                     *------------------------------------------*
           go to     acc-ckb-999.
       acc-ckb-999.
           exit.

      *================================================================*
      *    Accept numerico                                             *
      *----------------------------------------------------------------*
       acc-num-000.
      *              *-------------------------------------------------*
      *              * Pre-intervento per tipo campo 'V'               *
      *              *-------------------------------------------------*
           perform   pre-icv-000          thru pre-icv-999            .
       acc-num-050.
      *              *-------------------------------------------------*
      *              * Memorizzazione valore di default in work-area   *
      *              *-------------------------------------------------*
           move      v-num                to   w-num                  .
      *              *-------------------------------------------------*
      *              * Preparazione parametri per editing              *
      *              *-------------------------------------------------*
           perform   car-edn-000          thru car-edn-999            .
       acc-num-100.
      *              *-------------------------------------------------*
      *              * Esecuzione editing numerico                     *
      *              *-------------------------------------------------*
           perform   num-edt-000          thru num-edt-999            .
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione      *
      *              *-------------------------------------------------*
           move      v-edt                to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-edl                to   w-siz                  .
           inspect   w-fld   replacing leading spaces by w-unu        .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Inizializzazione campi di lavoro                *
      *              *-------------------------------------------------*
      *                     *------------------------------------------*
      *                     * Parte intera del numero                  *
      *                     *------------------------------------------*
           move      zero                 to   w-int
      *                     *------------------------------------------*
      *                     * Parte decimale del numero                *
      *                     *------------------------------------------*
                                               w-dec
      *                     *------------------------------------------*
      *                     * Numero di interi gia' impostati          *
      *                     *------------------------------------------*
                                               w-iim
      *                     *------------------------------------------*
      *                     * Numero di decimali gia' impostati        *
      *                     *------------------------------------------*
                                               w-dim
      *                     *------------------------------------------*
      *                     * Numero di virgole gia' impostate         *
      *                     *------------------------------------------*
                                               w-vim
      *                     *------------------------------------------*
      *                     * Numero di segni algebrici gia' impostati *
      *                     *------------------------------------------*
                                               w-sim
      *                     *------------------------------------------*
      *                     * Numero di caratteri 'E' gia' impostati   *
      *                     *------------------------------------------*
                                               w-eim
      *                     *------------------------------------------*
      *                     * Numero di caratteri 'L' gia' impostati   *
      *                     *------------------------------------------*
                                               w-lim                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione indicatore carattere attuale   *
      *              *-------------------------------------------------*
           move      1                    to   w-ccr                  .
       acc-num-200.
      *              *-------------------------------------------------*
      *              * Salvataggio carattere precedente                *
      *              *-------------------------------------------------*
           move      w-chr(w-ccr)         to   w-sav                  .
      *              *-------------------------------------------------*
      *              * Impostazione carattere attuale e determinazione *
      *              * del tasto di terminazione usato                 *
      *              *-------------------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
      *              *-------------------------------------------------*
      *              * Se terminatore va' a gestione terminatori       *
      *              *-------------------------------------------------*
           if        k-key                not  = spaces
                     go to acc-num-400.
      *              *-------------------------------------------------*
      *              * Controllo se carattere previsto (0123456789-,)  *
      *              * oppure 'E', 'e', 'L' o 'l'                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   w-cnp            tallying w-ctr
                     for   characters   before initial w-chr(w-ccr)   .
           if        w-ctr                <    12
                     go to acc-num-260
           else if   w-ctr                =    12
                     move  ","            to   w-chr(w-ccr)
                     go to acc-num-260
           else if   w-ctr                =    13 or
                     w-ctr                =    14
                     move  "E"            to   w-chr(w-ccr)
                     go to acc-num-240
           else if   w-ctr                =    15 or
                     w-ctr                =    16
                     move  "L"            to   w-chr(w-ccr)
                     go to acc-num-250.
       acc-num-230.
      *              *-------------------------------------------------*
      *              * Se carattere non accettabile ripristino il ca-  *
      *              * rattere precedentemente salvato                 *
      *              *-------------------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
           go to     acc-num-200.
       acc-num-240.
      *              *-------------------------------------------------*
      *              * Se impostata 'E'                                *
      *              *-------------------------------------------------*
           if        w-chr(w-ccr)         not  = "E"
                     go to acc-num-250.
           if        w-ccr                not  =  1 or
                     v-tip                not  = "V"
                     go to acc-num-230.
           move      1                    to   w-eim                  .
           move      zero                 to   w-num                  .
           add       2                    to   v-dec                  .
           go to     acc-num-290.
       acc-num-250.
      *              *-------------------------------------------------*
      *              * Se impostata 'L'                                *
      *              *-------------------------------------------------*
           if        w-chr(w-ccr)         not  = "L"
                     go to acc-num-260.
           if        w-ccr                not  =  1 or
                     v-tip                not  = "V"
                     go to acc-num-230.
           move      1                    to   w-lim                  .
           move      zero                 to   w-num                  .
           go to     acc-num-290.
       acc-num-260.
      *              *-------------------------------------------------*
      *              * Se impostato segno algebrico                    *
      *              *-------------------------------------------------*
           if        w-chr(w-ccr)         not  = "-"
                     go to acc-num-270.
           if        w-ccr                not  =  1 or
                     v-sgn                not  = "S"
                     go to acc-num-230.
           move      1                    to   w-sim                  .
           go to     acc-num-290.
       acc-num-270.
      *              *-------------------------------------------------*
      *              * Se impostato punto decimale                     *
      *              *-------------------------------------------------*
           if        w-chr(w-ccr)         not  = ","
                     go to acc-num-280.
           if        v-dec                not  > zero or
                     w-vim                not  = zero
                     go to acc-num-230.
           move      1                    to   w-vim                  .
           go to     acc-num-290.
       acc-num-280.
      *              *-------------------------------------------------*
      *              * Se impostata cifra 0-9                          *
      *              *-------------------------------------------------*
           if        w-vim                >    zero
                     go to acc-num-285.
      *              *-------------------------------------------------*
      *              * Cifra della parte intera                        *
      *              *-------------------------------------------------*
           if        w-iim                =    v-car
                     go to acc-num-230.
           add       1                    to   w-iim                  .
           multiply  10                   by   w-int                  .
           add       w-chn(w-ccr)         to   w-int                  .
           go to     acc-num-290.
       acc-num-285.
      *              *-------------------------------------------------*
      *              * Cifra della parte decimale                      *
      *              *-------------------------------------------------*
           if        w-dim                =    v-dec
                     go to acc-num-230.
           add       1                    to   w-dim                  .
           move      w-chn(w-ccr)         to   w-dcc(w-dim)           .
       acc-num-290.
      *              *-------------------------------------------------*
      *              * Se primo carattere impostato                    *
      *              *-------------------------------------------------*
           perform   vis-mec-000          thru vis-mec-999            .
           if        w-ccr                not  = 1
                     go to acc-num-294.
           move      w-chr(w-ccr)         to   w-sav                  .
           move      w-alu                to   w-fld                  .
           move      w-sav                to   w-chr(1)               .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-edl                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
       acc-num-294.
      *              *-------------------------------------------------*
      *              * Se carattere successivo al primo                *
      *              *-------------------------------------------------*
           if        w-ccr                not  = v-edl
                     add   1              to   w-ccr
                                               w-pos                  .
           go to     acc-num-200.
       acc-num-400.
      *              *-------------------------------------------------*
      *              * Ripristino carattere precedentemente salvato    *
      *              *-------------------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
      *              *-------------------------------------------------*
      *              * Composizione valore effettivamente impostato o  *
      *              * ritenzione del valore precedente se non e' sta- *
      *              * to impostato nessun carattere                   *
      *              *-------------------------------------------------*
           if        w-iim                >    zero or
                     w-dim                >    zero or
                     w-vim                >    zero or
                     w-sim                >    zero
                     move   w-dec         to   w-num
                     divide 100000        into w-num
                     add    w-int         to   w-num
                     if     w-sim         >    zero
                            multiply -1   by   w-num                  .
      *              *-------------------------------------------------*
      *              * Test se function-key prevista                   *
      *              *-------------------------------------------------*
           perform   tst-pfk-000          thru tst-pfk-999            .
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Left Arrow                   *
      *              *-------------------------------------------------*
           if        k-key                not  = "LEFT"
                     go to acc-num-500.
      *                     *------------------------------------------*
      *                     * Se il contenuto attuale e' uguale al va- *
      *                     * lore di default lo si annulla, in caso   *
      *                     * contrario si ripristina il valore di de- *
      *                     * fault                                    *
      *                     *------------------------------------------*
           if        w-num                =    v-num
                     move   zero          to   w-num
                     go to  acc-num-100
           else      go to  acc-num-050.
       acc-num-500.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Right Arrow : ignorato       *
      *              * Caso : Terminatore Append      : ignorato       *
      *              * Caso : Insert Character        : ignorato       *
      *              * Caso : Delete Character        : ignorato       *
      *              *-------------------------------------------------*
           if        k-key                =    "RGHT" or
                     k-key                =    "APND" or
                     k-key                =    "ICHR" or
                     k-key                =    "DCHR"
                     go to acc-num-200.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Return                       *
      *              *-------------------------------------------------*
           if        k-key                not  = "RTRN"
                     go to acc-num-700.
      *                     *------------------------------------------*
      *                     * Preparazione tasto di fine in uscita     *
      *                     *------------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-num-800.
       acc-num-700.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore non riconosciuto             *
      *              *-------------------------------------------------*
           if        k-key                =    high-value
                     go to acc-num-200.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore "COPY"                       *
      *              *-------------------------------------------------*
           if        k-key                =    "COPY"
                     move   w-num         to   z-cln
                     go to  acc-num-200.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore "PAST"                       *
      *              *-------------------------------------------------*
           if        k-key                not  = "PAST"
                     go to acc-num-750.
           move      z-cln                to   w-cnn                  .
           subtract  v-car                from 13
                                        giving w-pnt                  .
       acc-num-710.
           if        w-pnt                >    zero
                     move     zero        to   w-cnc(w-pnt)
                     subtract 1           from w-pnt
                     go to    acc-num-710.
           add       14
                     v-dec              giving w-pnt                  .
       acc-num-720.
           if        w-pnt                <    19
                     move     zero        to   w-cnc(w-pnt)
                     add      1           to   w-pnt
                     go to    acc-num-720.
           move      w-cnn                to   w-num                  .
           if        v-sgn                =    "S" and
                     z-cln                <    zero
                     multiply -1          by   w-num                  .
           go to     acc-num-100.
       acc-num-750.
      *              *-------------------------------------------------*
      *              * Caso : Altri terminatori riconosciuti           *
      *              *-------------------------------------------------*
           move      k-key                to   v-key                  .
       acc-num-800.
      *              *-------------------------------------------------*
      *              * Eventuale conversione in presenza del carattere *
      *              * 'E' (Euro convertitore)                         *
      *              *-------------------------------------------------*
           if        w-eim                =    zero
                     go to acc-num-810.
           multiply  1936,27              by   w-num                  .
           subtract  2                    from v-dec                  .
           go to     acc-num-820.
       acc-num-810.
      *              *-------------------------------------------------*
      *              * Eventuale conversione in presenza del carattere *
      *              * 'L' (Lire convertitore)                         *
      *              *-------------------------------------------------*
           if        w-lim                =    zero
                     go to acc-num-820.
           move      w-num                to   w-num-cnv              .
           divide    1936,27              into w-num-cnv rounded      .
           move      w-num-cnv            to   w-num                  .
           go to     acc-num-820.
       acc-num-820.
      *              *-------------------------------------------------*
      *              * Visualizzazione campo impostato ed aggiornamen- *
      *              * to buffer screen corrispondente, a meno che non *
      *              * siamo in 'no edit'                              *
      *              *-------------------------------------------------*
           perform   num-edt-000          thru num-edt-999            .
           if        w-noe                =    spaces
                     move     v-edt       to   w-fld
                     move     v-lin       to   w-lin
                     move     v-pos       to   w-pos
                     move     v-edl       to   w-siz
                     perform  vis-mef-000 thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Segnale se campo modificato                     *
      *              *-------------------------------------------------*
           if        w-num                =    v-num
                     move   spaces        to   v-mod
           else      move   "S"           to   v-mod                  .
      *              *-------------------------------------------------*
      *              * Preparazione campo in uscita                    *
      *              *-------------------------------------------------*
           move      w-num                to   v-num                  .
      *              *-------------------------------------------------*
      *              * Normalizzazione f-keys possibili                *
      *              *-------------------------------------------------*
           move      spaces               to   v-ufk                  .
      *              *-------------------------------------------------*
      *              * Post-intervento per tipo campo 'V''             *
      *              *-------------------------------------------------*
           perform   pos-icv-000          thru pos-icv-999            .
       acc-num-999.
           exit.

      *================================================================*
      *    Pre-intervento per tipo campo 'V'                           *
      *----------------------------------------------------------------*
       pre-icv-000.
      *              *-------------------------------------------------*
      *              * Test se da effettuare                           *
      *              *-------------------------------------------------*
           if       (v-tip                not  = "V"    ) or
                    (v-ope                not  = "AC" and
                     v-ope                not  = "DS" and
                     v-ope                not  = "ED" and
                     v-ope                not  = "PF"   )
                     move  spaces         to   w-tcv
                     go to pre-icv-999.
           move      "#"                  to   w-tcv                  .
           subtract  v-dec                from v-car                  .
           if        v-dec                =    1
                     divide 10            into v-num
           else if   v-dec                =    2
                     divide 100           into v-num
           else if   v-dec                =    3
                     divide 1000          into v-num
           else if   v-dec                =    4
                     divide 10000         into v-num
           else if   v-dec                =    5
                     divide 100000        into v-num                  .
       pre-icv-999.
           exit.

      *================================================================*
      *    Post-intervento per tipo campo 'V'                          *
      *----------------------------------------------------------------*
       pos-icv-000.
      *              *-------------------------------------------------*
      *              * Test se da effettuare                           *
      *              *-------------------------------------------------*
           if        w-tcv                =    spaces
                     go to pos-icv-999.
           add       v-dec                to   v-car                  .
           if        v-dec                =    1
                     multiply 10          by   v-num
           else if   v-dec                =    2
                     multiply 100         by   v-num
           else if   v-dec                =    3
                     multiply 1000        by   v-num
           else if   v-dec                =    4
                     multiply 10000       by   v-num
           else if   v-dec                =    5
                     multiply 100000      by   v-num                  .
       pos-icv-999.
           exit.

      *================================================================*
      *    Accept progressivo/anno                                     *
      *----------------------------------------------------------------*
       acc-pga-000.
      *              *-------------------------------------------------*
      *              * Salvataggio valore iniziale non normalizzato    *
      *              *-------------------------------------------------*
           move      v-num                to   w-psg                  .
      *              *-------------------------------------------------*
      *              * Determinazione lunghezza campo editato          *
      *              *-------------------------------------------------*
           add       3
                     v-car              giving w-prl                  .
      *              *-------------------------------------------------*
      *              * Determinazione indice per unstring              *
      *              *-------------------------------------------------*
           subtract  v-car                from 11
                                        giving w-pri                  .
      *              *-------------------------------------------------*
      *              * Determinazione moltiplicatore divisore          *
      *              *-------------------------------------------------*
           move      1                    to   w-pmd                  .
           move      v-car                to   w-ctr                  .
       acc-pga-025.
           if        w-ctr                >    zero
                     multiply 10          by   w-pmd
                     subtract 1           from w-ctr
                     go to    acc-pga-025.
      *              *-------------------------------------------------*
      *              * Separazione campo numerico v-num in componenti  *
      *              * - w-prs : secolo (*)                            *
      *              * - w-pra : anno                                  *
      *              * - w-prn : numero progressivo                    *
      *              * ----------------------------------------------- *
      *              *                                                 *
      *              * (*) La normalizzazione del secolo nella data    *
      *              *     avviene come segue : se l'anno e' minore di *
      *              *     85 si assume il secolo relativo al 2000,    *
      *              *     altrimenti si assume il secolo relativo al  *
      *              *     1900. Se pero' l'intera data in formato     *
      *              *     'aa.mm.gg' e' a zero, sipone a zero anche   *
      *              *     il secolo                                   *
      *              *                                                 *
      *              *-------------------------------------------------*
           move      v-num                to   w-int                  .
           divide    w-pmd                into w-int
                                        giving w-ctr
                                     remainder w-prn                  .
           move      w-ctr                to   w-pra                  .
           if        w-prn                =    zero and
                     w-pra                =    zero
                     perform est-sdt-000  thru est-sdt-999
                     move    i-ann        to   w-pra                  .
           if        w-pra                <    85
                     move    1            to   w-prs
           else      move    zero         to   w-prs                  .
      *              *-------------------------------------------------*
      *              * Salvataggio valore iniziale normalizzato        *
      *              *-------------------------------------------------*
           move      w-prt                to   w-psv                  .
       acc-pga-100.
      *              *-------------------------------------------------*
      *              * Editing valore di default                       *
      *              *-------------------------------------------------*
           move      w-prn                to   w-ped-pro              .
           move      "/"                  to   w-ped-bar              .
           move      w-pra                to   w-ped-ann              .
           inspect   w-ped   replacing leading spaces by w-unu        .
      *              *-------------------------------------------------*
      *              * Visualizzazione valore di default               *
      *              *-------------------------------------------------*
           move      w-pri                to   w-pnt                  .
           unstring  w-ped                into w-fld
                                  with pointer w-pnt                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      w-prl                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
       acc-pga-150.
      *              *-------------------------------------------------*
      *              * Impostazione parte progressivo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione numero caratteri impostati *
      *                  *---------------------------------------------*
           move      zero                 to   w-iim                  .
      *                  *---------------------------------------------*
      *                  * Inizializzazione indicat. carattere attuale *
      *                  *---------------------------------------------*
           move      1                    to   w-ccr                  .
       acc-pga-200.
      *                  *---------------------------------------------*
      *                  * Salvataggio carattere precedente            *
      *                  *---------------------------------------------*
           move      w-chr(w-ccr)         to   w-sav                  .
      *                  *---------------------------------------------*
      *                  * Impostazione carattere attuale e determina- *
      *                  * zione del tasto di terminazione usato       *
      *                  *---------------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
      *                  *---------------------------------------------*
      *                  * Se terminatore va' a gestione terminatori   *
      *                  *---------------------------------------------*
           if        k-key                not  = spaces
                     go to acc-pga-400.
      *                  *---------------------------------------------*
      *                  * Controllo se carattere previsto             *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  < "0" and
                     w-chr(w-ccr)         not  > "9"
                     go to acc-pga-300.
           if        w-chr(w-ccr)         =    "/"   or
                     w-chr(w-ccr)         =    ","   or
                     w-chr(w-ccr)         =    "."   or
                     w-chr(w-ccr)         =    "-"
                     go to acc-pga-550.
       acc-pga-250.
      *                  *---------------------------------------------*
      *                  * Se carattere non accettabile ripristino il  *
      *                  * carattere precedentemente salvato           *
      *                  *---------------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
           go to     acc-pga-200.
       acc-pga-300.
      *                  *---------------------------------------------*
      *                  * Se impostata cifra 0-9                      *
      *                  *---------------------------------------------*
           if        w-iim                not  < v-car
                     go to acc-pga-250.
           add       1                    to   w-iim                  .
           if        w-iim                >    1
                     go to acc-pga-350.
      *                      *-----------------------------------------*
      *                      * Se primo carattere impostato            *
      *                      *-----------------------------------------*
           move      w-chr(w-ccr)         to   w-sav                  .
           inspect   w-ped           replacing characters by w-unu
                     before            initial "/"                    .
           move      w-pri                to   w-pnt                  .
           unstring  w-ped                into w-fld
                                  with pointer w-pnt                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      w-prl                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
           move      w-sav                to   w-chr(w-ccr)           .
           move      zero                 to   w-prn                  .
       acc-pga-350.
      *                      *-----------------------------------------*
      *                      * Se carattere successivo al primo        *
      *                      *-----------------------------------------*
           perform   vis-mec-000          thru vis-mec-999            .
           multiply  10                   by   w-prn                  .
           add       w-chn(w-ccr)         to   w-prn                  .
           add       1                    to   w-ccr
                                               w-pos                  .
           go to     acc-pga-200.
       acc-pga-400.
      *                  *---------------------------------------------*
      *                  * Se impostato un terminatore                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ripristino il carattere precedentemente *
      *                      * salvato                                 *
      *                      *-----------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
      *                      *-----------------------------------------*
      *                      * Test se function-key prevista           *
      *                      *-----------------------------------------*    
           perform   tst-pfk-000          thru tst-pfk-999            .
      *                          *-------------------------------------*
      *                          * Caso : Terminatore Right Arrow      *
      *                          *-------------------------------------*
           if        k-key                =    "RGHT"
                     go to acc-pga-600.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore Append           *
      *                          *-------------------------------------*
           if        k-key                =    "APND"
                     go to acc-pga-200.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore Left Arrow       *
      *                          *-------------------------------------*
           if        k-key                =    "LEFT"
                     if   w-prt           =    w-psv
                          move  zero      to   w-prn
                          go to acc-pga-100
                     else move  w-psv     to   w-prt
                          go to acc-pga-100.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore Return           *
      *                          *-------------------------------------*
           if        k-key                not  = "RTRN"
                     go to acc-pga-500.
           move      spaces               to   k-key                  .
       acc-pga-450.
      *                              *---------------------------------*
      *                              * Normalizzazione valore          *
      *                              *                                 *
      *                              * Vedi note (*)                   *
      *                              *---------------------------------*
           if        w-prn                =    zero
                     move  zero           to   w-prs
                                               w-pra
           else      if    w-pra          <    85
                           move   1       to   w-prs
                     else  move   zero    to   w-prs                  .
      *                              *---------------------------------*
      *                              * Composizione valore in uscita   *
      *                              *---------------------------------*
           move      w-prs                to   w-int                  .
           multiply  100                  by   w-int                  .
           add       w-pra                to   w-int                  .
           multiply  w-pmd                by   w-int                  .
           add       w-prn                to   w-int                  .
           move      w-int                to   v-num                  .
      *                              *---------------------------------*
      *                              * Visualizzazione finale          *
      *                              *---------------------------------*
           perform   dsp-pga-000          thru dsp-pga-999            .
      *                              *---------------------------------*
      *                              * Segnale se campo modificato     *
      *                              *---------------------------------*
           if        v-num                =    w-psg
                     move  spaces         to   v-mod
           else      move   "S"           to   v-mod                  .
      *                              *---------------------------------*
      *                              * Function-key in uscita          *
      *                              *---------------------------------*
           move      k-key                to   v-key                  .
      *                              *---------------------------------*
      *                              * Normalizzazione f-keys          *
      *                              *---------------------------------*
           move      spaces               to   v-ufk                  .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     acc-pga-999.
       acc-pga-500.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore non riconosciuto *
      *                          *-------------------------------------*
           if        k-key                =    "ICHR" or
                     k-key                =    "DCHR"
                     move  high-value     to   k-key                  .
           if        k-key                =    high-value
                     go to acc-pga-200.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore "COPY"           *
      *                          *-------------------------------------*
           if        k-key                =    "COPY"
                     move   w-prn         to   z-cln
                     go to  acc-pga-200.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore "PAST"           *
      *                          *-------------------------------------*
           if        k-key                =    "PAST"
                     move  z-cln          to   w-prn
                     go to acc-pga-100.
      *                          *-------------------------------------*
      *                          * Caso : Altri terminatori previsti   *
      *                          *-------------------------------------*
           go to     acc-pga-450.
       acc-pga-550.
      *                          *-------------------------------------*
      *                          * Ripristino il carattere precedente- *
      *                          * mente salvato                       *
      *                          *-------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
       acc-pga-600.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore Right arrow      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se il valore della parte pro-   *
      *                              * gressiva e' zero non si accetta *
      *                              * lo spostamento sulla parte anno *
      *                              *---------------------------------*
           if        w-prn                =    zero
                     go to acc-pga-200.
      *                              *---------------------------------*
      *                              * Editing parte progressiva       *
      *                              *---------------------------------*
           move      w-prn                to   w-ped-pro              .
           move      w-pri                to   w-pnt                  .
           unstring  w-ped                into w-fld
                                  with pointer w-pnt                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-car                to   w-siz                  .
           perform   vis-mef-000          thru vis-mef-999            .
       acc-pga-650.
      *              *-------------------------------------------------*
      *              * Impostazione parte anno                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anno in area editata                        *
      *                  *---------------------------------------------*
           move      w-pra                to   w-fld                  .
      *                  *---------------------------------------------*
      *                  * Inizializzazione numero caratteri impostati *
      *                  *---------------------------------------------*
           move      zero                 to   w-iim                  .
      *                  *---------------------------------------------*
      *                  * Inizializzazione indicat. carattere attuale *
      *                  *---------------------------------------------*
           move      1                    to   w-ccr                  .
      *                  *---------------------------------------------*
      *                  * Inizializzazione posizione attuale          *
      *                  *---------------------------------------------*
           move      v-pos                to   w-pos                  .
           add       v-car                to   w-pos                  .
           add       1                    to   w-pos                  .
       acc-pga-700.
      *                  *---------------------------------------------*
      *                  * Salvataggio carattere precedente            *
      *                  *---------------------------------------------*
           move      w-chr(w-ccr)         to   w-sav                  .
      *                  *---------------------------------------------*
      *                  * Impostazione carattere attuale e determina- *
      *                  * zione del tasto di terminazione usato       *
      *                  *---------------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
      *                  *---------------------------------------------*
      *                  * Se terminatore va' a gestione terminatori   *
      *                  *---------------------------------------------*
           if        k-key                not  = spaces
                     go to acc-pga-850.
      *                  *---------------------------------------------*
      *                  * Controllo se carattere previsto             *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  < "0" and
                     w-chr(w-ccr)         not  > "9"
                     go to acc-pga-800.
       acc-pga-750.
      *                  *---------------------------------------------*
      *                  * Se carattere non accettabile ripristino il  *
      *                  * carattere precedentemente salvato           *
      *                  *---------------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
           go to     acc-pga-700.
       acc-pga-800.
      *                  *---------------------------------------------*
      *                  * Se impostata cifra 0-9                      *
      *                  *---------------------------------------------*
           if        w-iim                not  < 2
                     go to acc-pga-750.
           add       1                    to   w-iim                  .
           move      w-chn (w-ccr)        to   w-pax (w-iim)          .
           perform   vis-mec-000          thru vis-mec-999            .
           if        w-iim                <    2
                     add   1              to   w-ccr
                                               w-pos                  .
           go to     acc-pga-700.
       acc-pga-850.
      *                  *---------------------------------------------*
      *                  * Se impostato un terminatore                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ripristino il carattere precedentemente *
      *                      * salvato                                 *
      *                      *-----------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
      *                      *-----------------------------------------*
      *                      * Test se function-key prevista           *
      *                      *-----------------------------------------*    
           perform   tst-pfk-000          thru tst-pfk-999            .
      *                          *-------------------------------------*
      *                          * Caso : Terminatore Right Arrow      *
      *                          *-------------------------------------*
           if        k-key                =    "RGHT"
                     go to acc-pga-700.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore Append           *
      *                          *-------------------------------------*
           if        k-key                =    "APND"
                     go to acc-pga-700.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore Left Arrow       *
      *                          *-------------------------------------*
           if        k-key                =    "LEFT"
                     if   w-iim           =    zero
                          go to acc-pga-100
                     else go to acc-pga-650.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore Return           *
      *                          *-------------------------------------*
           if        k-key                =    "RTRN"
                     move  spaces         to   k-key
                     go to acc-pga-450.
       acc-pga-900.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore non riconosciuto *
      *                          *-------------------------------------*
           if        k-key                =    "ICHR" or
                     k-key                =    "DCHR"
                     move  high-value     to   k-key                  .
           if        k-key                =    high-value
                     go to acc-pga-700.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore "COPY"           *
      *                          *-------------------------------------*
           if        k-key                =    "COPY"
                     move   w-pra         to   z-cln
                     go to  acc-pga-700.
      *                          *-------------------------------------*
      *                          * Caso : Terminatore "PAST"           *
      *                          *-------------------------------------*
           if        k-key                =    "PAST"
                     move    z-cln        to   w-pra
                     move    w-pra        to   w-fld
                     move    v-lin        to   w-lin
                     move    v-pos        to   w-pos
                     add     v-car        to   w-pos
                     add     1            to   w-pos
                     move    2            to   w-siz
                     perform vis-mef-000  thru vis-mef-999
                     go to acc-pga-700.
      *                          *-------------------------------------*
      *                          * Caso : Altri terminatori previsti   *
      *                          *-------------------------------------*
           go to     acc-pga-450.
       acc-pga-999.
           exit.

      *================================================================*
      *    Accept data                                                 *
      *----------------------------------------------------------------*
       acc-dat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione indicatore giorno della setti-  *
      *              * mana                                            *
      *              *-------------------------------------------------*
           move      spaces               to   w-dow                  .
      *              *-------------------------------------------------*
      *              * Se ammessa anche data maggiore di oggi          *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all ">"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-dma
           else      move   ">"           to   w-dma                  .
       acc-dat-050.
      *              *-------------------------------------------------*
      *              * Salvataggio valore in entrata                   *
      *              *-------------------------------------------------*
           move      v-dat                to   w-svd                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione valore di default in work-area   *
      *              *-------------------------------------------------*
           move      v-dat                to   w-amg                  .
       acc-dat-100.
      *              *-------------------------------------------------*
      *              * Inversione data da w-amg a w-dat                *
      *              *-------------------------------------------------*
           perform   dat-inv-000          thru dat-inv-999            .
      *              *-------------------------------------------------*
      *              * Esecuzione editing data                         *
      *              *-------------------------------------------------*
           perform   dat-edt-000          thru dat-edt-999            .
      *              *-------------------------------------------------*
      *              * Preparazione parametri per visualizzazione      *
      *              *-------------------------------------------------*
           if        v-edt                =    spaces
                     if     w-unu         =    "_"
                            move  "__/__/__"
                                          to   w-fld
                     else   move  "../../.."
                                          to   w-fld
           else      move   v-edt         to   w-fld                  .
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      v-edl                to   w-siz                  .
      *              *-------------------------------------------------*
      *              * Subroutine di visualizzazione e bufferizzazione *
      *              *-------------------------------------------------*
           perform   vis-mef-000          thru vis-mef-999            .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero caratteri impostati     *
      *              *-------------------------------------------------*
           move      zero                 to   w-iim                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione indicatore carattere attuale   *
      *              *-------------------------------------------------*
           move      1                    to   w-ccr                  .
       acc-dat-200.
      *              *-------------------------------------------------*
      *              * Salvataggio carattere precedente                *
      *              *-------------------------------------------------*
           move      w-chr(w-ccr)         to   w-sav                  .
      *              *-------------------------------------------------*
      *              * Impostazione carattere attuale e determinazione *
      *              * del tasto di terminazione usato                 *
      *              *-------------------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
      *              *-------------------------------------------------*
      *              * Se terminatore va' a gestione terminatori       *
      *              *-------------------------------------------------*
           if        k-key                not  = spaces
                     go to acc-dat-400.
       acc-dat-203.
      *              *-------------------------------------------------*
      *              * Controllo se carattere previsto (0123456789)    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se primo carattere                     *
      *                  *---------------------------------------------*
           if        w-ccr                >    1
                     go to acc-dat-205.
      *                  *---------------------------------------------*
      *                  * Trasformazione caratteri speciali           *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         =    "4"
                     move  "I"            to   w-chr(w-ccr)
                     go to acc-dat-240.
           if        w-chr(w-ccr)         =    "5"
                     move  "O"            to   w-chr(w-ccr)
                     go to acc-dat-230.
           if        w-chr(w-ccr)         =    "6"
                     move  "M"            to   w-chr(w-ccr)
                     go to acc-dat-225.
           if        w-chr(w-ccr)         =    "7"
                     move  "P"            to   w-chr(w-ccr)
                     go to acc-dat-210.
           if        w-chr(w-ccr)         =    "8"
                     move  "U"            to   w-chr(w-ccr)
                     go to acc-dat-220.
           if        w-chr(w-ccr)         =    "9"
                     move  "Y"            to   w-chr(w-ccr)
                     go to acc-dat-235.
       acc-dat-205.
      *                  *---------------------------------------------*
      *                  * Test se caratteri numerici                  *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  < "0" and
                     w-chr(w-ccr)         not  > "9"
                     go to acc-dat-350.
       acc-dat-210.
      *              *-------------------------------------------------*
      *              * Se "P" : emissione della data di inizio anno    *
      *              *          precedente, solo se e' il primo carat- *
      *              *          tere impostato                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se primo carattere                     *
      *                  *---------------------------------------------*
           if        w-ccr                >    1
                     go to acc-dat-300.
      *                  *---------------------------------------------*
      *                  * Test su carattere impostato                 *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  = "p" and
                     w-chr(w-ccr)         not  = "P"
                     go to acc-dat-220.
      *                  *---------------------------------------------*
      *                  * Estrazione system date                      *
      *                  *---------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
      *                  *---------------------------------------------*
      *                  * Data estratta in campo di uscita            *
      *                  *---------------------------------------------*
           move      i-dat                to   v-dat                  .
           move      01                   to   v-gio                  .
           move      01                   to   v-mes                  .
           subtract  01                   from v-ann                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore estratto         *
      *                  *---------------------------------------------*
           perform   dsp-dat-000          thru dsp-dat-999            .
      *                  *---------------------------------------------*
      *                  * Forzatura terminatore                       *
      *                  *---------------------------------------------*
           move      "RTRN"               to   k-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo formale della data              *
      *                  *---------------------------------------------*
           go to     acc-dat-460.
       acc-dat-220.
      *              *-------------------------------------------------*
      *              * Se "U" : emissione della data di fine anno pre- *
      *              *          cedente, solo se e' il primo carattere *
      *              *          impostato                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su carattere impostato                 *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  = "u" and
                     w-chr(w-ccr)         not  = "U"
                     go to acc-dat-225.
      *                  *---------------------------------------------*
      *                  * Estrazione system date                      *
      *                  *---------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
      *                  *---------------------------------------------*
      *                  * Data estratta in campo di uscita            *
      *                  *---------------------------------------------*
           move      i-dat                to   v-dat                  .
           move      31                   to   v-gio                  .
           move      12                   to   v-mes                  .
           subtract  01                   from v-ann                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore estratto         *
      *                  *---------------------------------------------*
           perform   dsp-dat-000          thru dsp-dat-999            .
      *                  *---------------------------------------------*
      *                  * Forzatura terminatore                       *
      *                  *---------------------------------------------*
           move      "RTRN"               to   k-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo formale della data              *
      *                  *---------------------------------------------*
           go to     acc-dat-460.
       acc-dat-225.
      *              *-------------------------------------------------*
      *              * Se "M" : emissione della data di inizio mese in *
      *              *          corso, solo se e' il primo carattere   *
      *              *          impostato                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su carattere impostato                 *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  = "m" and
                     w-chr(w-ccr)         not  = "M"
                     go to acc-dat-230.
      *                  *---------------------------------------------*
      *                  * Estrazione system date                      *
      *                  *---------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
      *                  *---------------------------------------------*
      *                  * Data estratta in campo di uscita            *
      *                  *---------------------------------------------*
           move      i-dat                to   v-dat                  .
           move      01                   to   v-gio                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore estratto         *
      *                  *---------------------------------------------*
           perform   dsp-dat-000          thru dsp-dat-999            .
      *                  *---------------------------------------------*
      *                  * Forzatura terminatore                       *
      *                  *---------------------------------------------*
           move      "RTRN"               to   k-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo formale della data              *
      *                  *---------------------------------------------*
           go to     acc-dat-460.
       acc-dat-230.
      *              *-------------------------------------------------*
      *              * Se "O" : emissione della data di sistema, solo  *
      *              *          se e' il primo carattere impostato     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su carattere impostato                 *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  = "o" and
                     w-chr(w-ccr)         not  = "O"
                     go to acc-dat-235.
      *                  *---------------------------------------------*
      *                  * Estrazione system date                      *
      *                  *---------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
      *                  *---------------------------------------------*
      *                  * Data estratta in campo di uscita            *
      *                  *---------------------------------------------*
           move      i-dat                to   v-dat                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore estratto         *
      *                  *---------------------------------------------*
           perform   dsp-dat-000          thru dsp-dat-999            .
      *                  *---------------------------------------------*
      *                  * Forzatura terminatore                       *
      *                  *---------------------------------------------*
           move      "RTRN"               to   k-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo formale della data              *
      *                  *---------------------------------------------*
           go to     acc-dat-460.
       acc-dat-235.
      *              *-------------------------------------------------*
      *              * Se "Y" : emissione della data del giorno prece- *
      *              *          dente, solo se e' il primo carattere   *
      *              *          impostato                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su carattere impostato                 *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  = "y" and
                     w-chr(w-ccr)         not  = "Y"
                     go to acc-dat-240.
      *                  *---------------------------------------------*
      *                  * Estrazione system date                      *
      *                  *---------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
      *                  *---------------------------------------------*
      *                  * Decremento di 1 giorno                      *
      *                  *---------------------------------------------*
           move      i-dat                to   w-det-nrg-dat-dtb      .
           move      01                   to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
      *                  *---------------------------------------------*
      *                  * Data calcolata in campo di uscita           *
      *                  *---------------------------------------------*
           move      w-det-nrg-dat-dtd    to   v-dat                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore estratto         *
      *                  *---------------------------------------------*
           perform   dsp-dat-000          thru dsp-dat-999            .
      *                  *---------------------------------------------*
      *                  * Forzatura terminatore                       *
      *                  *---------------------------------------------*
           move      "RTRN"               to   k-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo formale della data              *
      *                  *---------------------------------------------*
           go to     acc-dat-460.
       acc-dat-240.
      *              *-------------------------------------------------*
      *              * Se "I" : emissione della data di inizio anno in *
      *              *          corso, solo se e' il primo carattere   *
      *              *          impostato                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su carattere impostato                 *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  = "i" and
                     w-chr(w-ccr)         not  = "I"
                     go to acc-dat-245.
      *                  *---------------------------------------------*
      *                  * Estrazione system date                      *
      *                  *---------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
      *                  *---------------------------------------------*
      *                  * Data estratta in campo di uscita            *
      *                  *---------------------------------------------*
           move      i-dat                to   v-dat                  .
           move      01                   to   v-gio                  .
           move      01                   to   v-mes                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore estratto         *
      *                  *---------------------------------------------*
           perform   dsp-dat-000          thru dsp-dat-999            .
      *                  *---------------------------------------------*
      *                  * Forzatura terminatore                       *
      *                  *---------------------------------------------*
           move      "RTRN"               to   k-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo formale della data              *
      *                  *---------------------------------------------*
           go to     acc-dat-460.
       acc-dat-245.
      *              *-------------------------------------------------*
      *              * Se "G" : indicazione del giorno della settimana *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su carattere impostato                 *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  = "g" and
                     w-chr(w-ccr)         not  = "G"
                     go to acc-dat-260.
           if        v-dat                =    zero
                     go to acc-dat-300.
      *                  *---------------------------------------------*
      *                  * Test su indicatore giorno della settimana   *
      *                  *---------------------------------------------*
           if        w-dow                not  = spaces
                     go to acc-dat-247.
      *                  *---------------------------------------------*
      *                  * Determinazione literal giorno della setti-  *
      *                  * mana                                        *
      *                  *---------------------------------------------*
           move      v-dat                to   w-det-dow-dat          .
           perform   det-dow-lit-000      thru det-dow-lit-999        .
           move      w-det-dow-lit        to   v-alf                  .
           move      08                   to   v-car                  .
           perform   dsp-alf-000          thru dsp-alf-999            .
      *                  *---------------------------------------------*
      *                  * Attivazione dell' indicatore giorno della   *
      *                  * settimana                                   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-dow                  .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-dat-300.
       acc-dat-247.
      *                  *---------------------------------------------*
      *                  * Normalizzazione indicatore giorno della     *
      *                  * settimana                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-dow                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore estratto         *
      *                  *---------------------------------------------*
           perform   dsp-dat-000          thru dsp-dat-999            .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-dat-300.
       acc-dat-260.
      *              *-------------------------------------------------*
      *              * Se "+" : incremento data di un giorno           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su carattere impostato                 *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  = "+"
                     go to acc-dat-270.
           if        v-dat                =    zero
                     go to acc-dat-300.
      *                  *---------------------------------------------*
      *                  * Incremento di 1 giorno                      *
      *                  *---------------------------------------------*
           move      v-dat                to   w-det-dat-nrg-dtb      .
           move      01                   to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
      *                  *---------------------------------------------*
      *                  * Data calcolata in campo di uscita           *
      *                  *---------------------------------------------*
           move      w-det-dat-nrg-dti    to   v-dat                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore estratto         *
      *                  *---------------------------------------------*
           perform   dsp-dat-000          thru dsp-dat-999            .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-dat-300.
       acc-dat-270.
      *              *-------------------------------------------------*
      *              * Se "-" : decremento data di un giorno           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su carattere impostato                 *
      *                  *---------------------------------------------*
           if        w-chr(w-ccr)         not  = "-"
                     go to acc-dat-300.
           if        v-dat                =    zero
                     go to acc-dat-300.
      *                  *---------------------------------------------*
      *                  * Decremento di 1 giorno                      *
      *                  *---------------------------------------------*
           move      v-dat                to   w-det-nrg-dat-dtb      .
           move      01                   to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
      *                  *---------------------------------------------*
      *                  * Data calcolata in campo di uscita           *
      *                  *---------------------------------------------*
           move      w-det-nrg-dat-dtd    to   v-dat                  .
      *                  *---------------------------------------------*
      *                  * Visualizzazione del valore estratto         *
      *                  *---------------------------------------------*
           perform   dsp-dat-000          thru dsp-dat-999            .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-dat-300.
       acc-dat-300.
      *              *-------------------------------------------------*
      *              * Se carattere non accettabile ripristino il ca-  *
      *              * rattere precedentemente salvato                 *
      *              *-------------------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
           go to     acc-dat-200.
       acc-dat-350.
      *              *-------------------------------------------------*
      *              * Se impostata cifra 0-9                          *
      *              *-------------------------------------------------*
           if        w-iim                =    6
                     go to acc-dat-210.
           add       1                    to   w-iim                  .
           if        w-iim                >    1
                     go to acc-dat-370.
      *              *-------------------------------------------------*
      *              * Se primo carattere impostato                    *
      *              *-------------------------------------------------*
           if        w-dat                =    zero
                     go to acc-dat-370.
           move      w-chr(w-ccr)         to   w-sav                  .
           move      zero                 to   w-dat                  .
           if        w-unu                =    "_"
                     move  "__/__/__"     to   w-fld
           else      move  "../../.."     to   w-fld                  .
           move      v-pos                to   w-pos                  .
           perform   vis-mef-000          thru vis-mef-999            .
           move      w-sav                to   w-chr(w-ccr)           .
       acc-dat-370.
      *              *-------------------------------------------------*
      *              * Se carattere successivo al primo                *
      *              *-------------------------------------------------*
           move      w-chn(w-ccr)         to   w-dtc(w-iim)           .
           perform   vis-mec-000          thru vis-mec-999            .
           if        w-ccr                =    1   or
                     w-ccr                =    4   or
                     w-ccr                =    7
                     add    1             to   w-ccr
                                               w-pos
           else if   w-ccr                =    2   or
                     w-ccr                =    5
                     add    2             to   w-ccr
                                               w-pos                  .
           go to     acc-dat-200.
       acc-dat-400.
      *              *-------------------------------------------------*
      *              * Ripristino carattere precedentemente salvato    *
      *              *-------------------------------------------------*
           move      w-sav                to   w-chr(w-ccr)           .
      *              *-------------------------------------------------*
      *              * Test se function-key prevista                   *
      *              *-------------------------------------------------*
           perform   tst-pfk-000          thru tst-pfk-999            .
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Right Arrow : ignorato       *
      *              * Caso : Terminatore Append      : ignorato       *
      *              * Caso : Insert Character        : ignorato       *
      *              * Caso : Delete Character        : ignorato       *
      *              *-------------------------------------------------*
           if        k-key                =    "RGHT" or
                     k-key                =    "APND" or
                     k-key                =    "ICHR" or
                     k-key                =    "DCHR"
                     go to acc-dat-200.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Left Arrow                   *
      *              *-------------------------------------------------*
           if        k-key                not  = "LEFT"
                     go to acc-dat-450.
      *                     *------------------------------------------*
      *                     * Se il contenuto attuale e' uguale al va- *
      *                     * lore di default lo si annulla, in caso   *
      *                     * contrario si ripristina il valore di de- *
      *                     * fault                                    *
      *                     *------------------------------------------*
           if        w-aaa                =    v-ann and
                     w-mmm                =    v-mes and
                     w-ggg                =    v-gio
                     move   zero          to   w-amg
                     go to  acc-dat-100
           else      go to  acc-dat-050.
       acc-dat-450.
      *              *-------------------------------------------------*
      *              * Se il terminatore non e' stato impostato dopo   *
      *              * aver digitato zero o 4 o 6 caratteri, esso e'   *
      *              * ignorato                                        *
      *              *-------------------------------------------------*
           if        w-iim                not  = zero and
                     w-iim                not  = 4    and
                     w-iim                not  = 6
                     go to acc-dat-200.
      *              *-------------------------------------------------*
      *              * Se sono stati impostati solo quattro caratteri  *
      *              * si determina automaticamente l'anno             *
      *              *-------------------------------------------------*
      *                 *----------------------------------------------*
      *                 * Test su numero caratteri impostati           *
      *                 *----------------------------------------------*
           if        w-iim                not  = 4
                     go to acc-dat-460.
      *                 *----------------------------------------------*
      *                 * Estrazione data di sistema                   *
      *                 *----------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
           move      i-dtc                to   w-amg                  .
      *                 *----------------------------------------------*
      *                 * Anno da data attuale                         *
      *                 *----------------------------------------------*
           move      w-ann                to   w-aaa                  .
      *                 *----------------------------------------------*
      *                 * Se il mese impostato e' inferiore a quello   *
      *                 * della data attuale, forzatura anno e a con-  *
      *                 * trollo formale                               *
      *                 *----------------------------------------------*
           if        w-mmm                <    w-mes
                     go to acc-dat-460.
      *                 *----------------------------------------------*
      *                 * Differenza tra mese impostato e quello della *
      *                 * data attuale                                 *
      *                 *----------------------------------------------*
           subtract  w-mes                from w-mmm
                                        giving w-ctr                  .
      *                 *----------------------------------------------*
      *                 * Se il mese impostato e' superiore a quello   *
      *                 * della data attuale di 3 mesi, forzatura anno *
      *                 * in corso e a controllo formale               *
      *                 *----------------------------------------------*
           if        w-ctr                <    4
                     go to acc-dat-460.
      *                 *----------------------------------------------*
      *                 * Forzatura anno precedente                    *
      *                 *----------------------------------------------*
           if        w-aaa                =    zero
                     move     99          to   w-aaa
           else      subtract 1           from w-aaa                  .
       acc-dat-460.
      *              *-------------------------------------------------*
      *              * Controllo formale sulla data                    *
      *              *-------------------------------------------------*
           if        w-dat                =    zero
                     go to acc-dat-500.
           if        w-ggg                <    01 or
                     w-ggg                >    31
                     go to acc-dat-200.
           if        w-mmm                <    01 or
                     w-mmm                >    12
                     go to acc-dat-200.
           if        w-mmm                not  = 02
                     go to acc-dat-470.
           move      w-aaa                to   w-ctr                  .
           divide    4                    into w-ctr                  .
           multiply  4                    by   w-ctr                  .
           if        w-ctr                =    w-aaa
                     move   29            to   w-tgx(2)
           else      move   28            to   w-tgx(2)               .
       acc-dat-470.
           if        w-ggg                >    w-tgx(w-mmm)
                     go to  acc-dat-200.
       acc-dat-500.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore Return                       *
      *              *-------------------------------------------------*
           if        k-key                not  = "RTRN"
                     go to acc-dat-700.
      *                     *------------------------------------------*
      *                     * Preparazione tasto di fine in uscita     *
      *                     *------------------------------------------*
           move      spaces               to   v-key                  .
       acc-dat-600.
      *                     *------------------------------------------*
      *                     * Se data a zero : no controlli            *
      *                     *------------------------------------------*
           move      zero                 to   i-scl                  .
           move      w-aaa                to   i-ann                  .
           move      w-mmm                to   i-mes                  .
           move      w-ggg                to   i-gio                  .
           if        i-dtc                =    zero
                     go to acc-dat-650.
      *                     *------------------------------------------*
      *                     * Normalizzazione secolo/anno              *
      *                     *------------------------------------------*
           perform   nor-sec-000          thru nor-sec-999            .
      *                     *------------------------------------------*
      *                     * Se ammessa data maggiore di oggi : no    *
      *                     * controlli                                *
      *                     *------------------------------------------*
           if        w-dma                not  = spaces
                     go to acc-dat-650.
      *                     *------------------------------------------*
      *                     * Confronto con la data di sistema         *
      *                     *------------------------------------------*
           move      i-dat                to   i-dts                  .
           perform   est-sdt-000          thru est-sdt-999            .
           if        i-dts                >    i-dat
                     move  w-aaa          to   w-ann
                     move  w-mmm          to   w-mes
                     move  w-ggg          to   w-gio
                     go to acc-dat-100.
       acc-dat-650.
      *                     *------------------------------------------*
      *                     * Preparazione campo in uscita             *
      *                     *------------------------------------------*
           move      w-aaa                to   v-ann                  .
           move      w-mmm                to   v-mes                  .
           move      w-ggg                to   v-gio                  .
           move      v-dat                to   i-dtc                  .
           perform   nor-sec-000          thru nor-sec-999            .
           move      i-dat                to   v-dat                  .
      *                     *------------------------------------------*
      *                     * Eventuale normalizzazione secolo/anno    *
      *                     *                                          *
      *                     * N.B.: Se la data e' a zero, anche il se- *
      *                     *       colo, gestito internamente, deve   *
      *                     *       essere a zero                      *
      *                     *------------------------------------------*
           if        v-gio                =    zero and
                     v-mes                =    zero and
                     v-ann                =    zero
                     move  zero           to   v-sec                  .
      *                     *------------------------------------------*
      *                     * Visualizzazione campo impostato ed ag-   *
      *                     * giornamento buffer screen corrispondente *
      *                     *------------------------------------------*
           perform   dat-edt-000          thru dat-edt-999            .
           move      v-edt                to   w-fld                  .
           move      v-pos                to   w-pos                  .
           perform   vis-mef-000          thru vis-mef-999            .
      *                     *------------------------------------------*
      *                     * Normalizzazione f-keys possibili         *
      *                     *------------------------------------------*
           move      spaces               to   v-ufk                  .
      *                     *------------------------------------------*
      *                     * Va' all'uscita dalla routine             *
      *                     *------------------------------------------*
           go to     acc-dat-900.
       acc-dat-700.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore non riconosciuto             *
      *              *-------------------------------------------------*
           if        k-key                =    high-value
                     go to acc-dat-200.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore "COPY"                       *
      *              *-------------------------------------------------*
           if        k-key                =    "COPY"
                     move   w-aaa         to   z-aaa
                     move   w-mmm         to   z-mmm
                     move   w-ggg         to   z-ggg
                     go to  acc-dat-200.
      *              *-------------------------------------------------*
      *              * Caso : Terminatore "PAST"                       *
      *              *-------------------------------------------------*
           if        k-key                =    "PAST"
                     move   z-aaa         to   w-ann
                     move   z-mmm         to   w-mes
                     move   z-ggg         to   w-gio
                     go to  acc-dat-100.
      *              *-------------------------------------------------*
      *              * Caso : Altri terminatori riconosciuti           *
      *              *-------------------------------------------------*
           move      k-key                to   v-key                  .
           go to     acc-dat-600.
       acc-dat-900.
      *              *-------------------------------------------------*
      *              * Segnale se campo modificato                     *
      *              *-------------------------------------------------*
           if        w-svd                =    v-dat
                     move   spaces        to   v-mod
           else      move   "S"           to   v-mod                  .
       acc-dat-999.
           exit.

      *================================================================*
      *    Accept function-key                                         *
      *----------------------------------------------------------------*
       acc-fky-000.
      *              *-------------------------------------------------*
      *              * Preparazioni iniziali                           *
      *              *-------------------------------------------------*
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      01                   to   w-ccr                  .
           move      b-chr(w-lin, w-pos)  to   w-sav                  .
       acc-fky-100.
      *              *-------------------------------------------------*
      *              * Impostazione carattere attuale e determinazione *
      *              * del tasto di terminazione usato                 *
      *              *-------------------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
      *              *-------------------------------------------------*
      *              * Ripristino carattere precedentemente salvato    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se word-key                            *
      *                  *---------------------------------------------*
           if        v-tip                =    "W"
                     go to acc-fky-150.
      *                  *---------------------------------------------*
      *                  * Se function-key                             *
      *                  *---------------------------------------------*
           move      w-sav                to   w-chr (w-ccr)          .
       acc-fky-150.
      *              *-------------------------------------------------*
      *              * Rivisualizzazione del carattere precedentemente *
      *              * salvato                                         *
      *              *-------------------------------------------------*
           perform   vis-mec-000          thru vis-mec-999            .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di accettazione *
      *              *-------------------------------------------------*
           if        v-ufk                =    spaces
                     go to acc-fky-200
           else      go to acc-fky-300.
       acc-fky-200.
      *              *-------------------------------------------------*
      *              * Se nessuna function-key prevista                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione eventuale del Return        *
      *                  *---------------------------------------------*
           if        k-key                =    "RTRN"
                     move  spaces         to   k-key                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           move      k-key                to   v-key                  .
           move      spaces               to   v-ufk                  .
           go to     acc-fky-999.
       acc-fky-300.
      *              *-------------------------------------------------*
      *              * Se qualche function-key prevista                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' stata impostata alcuna function   *
      *                  * key si ritorna all'impostazione             *
      *                  *---------------------------------------------*
           if        k-key                =    spaces
                     go to acc-fky-100.
      *                  *---------------------------------------------*
      *                  * Test se tra le function key previste c'e'   *
      *                  * anche il Return                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-inx                  .
       acc-fky-400.
           add       1                    to   w-inx                  .
           if        w-inx                >    40
                     go to acc-fky-800.
           if        v-pfk (w-inx)        not  = "RTRN"
                     go to acc-fky-400.
       acc-fky-500.
      *                  *---------------------------------------------*
      *                  * Se si'                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se e' stato impostato il Return lo si   *
      *                      * normalizza e si esce                    *
      *                      *-----------------------------------------*
           if        k-key                =    "RTRN"
                     move  spaces         to   v-key
                     move  spaces         to   v-ufk
                     go to acc-fky-999.
       acc-fky-600.
      *                      *-----------------------------------------*
      *                      * Altrimenti si controlla che la function *
      *                      * key impostata sia tra quelle previste   *
      *                      *-----------------------------------------*
           move      zero                 to   w-inx                  .
       acc-fky-700.
           add       1                    to   w-inx                  .
           if        w-inx                >    40
                     go to acc-fky-100.
           if        v-pfk (w-inx)        not  = k-key
                     go to acc-fky-700.
           move      k-key                to   v-key                  .
           move      spaces               to   v-ufk                  .
           go to     acc-fky-999.
       acc-fky-800.
      *                  *---------------------------------------------*
      *                  * Se no                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se e' stato digitato Return, si norma-  *
      *                      * lizza la function key e si esce         *
      *                      *-----------------------------------------*
           if        k-key                =    "RTRN"
                     move  spaces         to   v-key
                     move  spaces         to   v-ufk
                     go to acc-fky-999.
      *                      *-----------------------------------------*
      *                      * Altrimenti si controlla che la function *
      *                      * key impostata sia tra quelle previste   *
      *                      *-----------------------------------------*
           go to     acc-fky-600.
       acc-fky-999.
           exit.

      *================================================================*
      *    Accept word-key                                             *
      *----------------------------------------------------------------*
       acc-wky-000.
      *              *-------------------------------------------------*
      *              * Preparazioni iniziali                           *
      *              *-------------------------------------------------*
           move      v-lin                to   w-lin                  .
           move      v-pos                to   w-pos                  .
           move      01                   to   w-ccr                  .
           move      b-chr(w-lin, w-pos)  to   w-sav                  .
       acc-wky-100.
      *              *-------------------------------------------------*
      *              * Impostazione carattere attuale e determinazione *
      *              * del tasto di terminazione usato                 *
      *              *-------------------------------------------------*
           perform   acc-key-000          thru acc-key-999            .
      *              *-------------------------------------------------*
      *              * Carattere accettato in uscita                   *
      *              *-------------------------------------------------*
           move      w-chr (w-ccr)        to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Uscita con eventuale function-key               *
      *              *-------------------------------------------------*
           move      k-key                to   v-key                  .
       acc-wky-999.
           exit.

      *================================================================*
      * Sub-sub-routines                                               *
      *================================================================*
           
      *================================================================*
      * Visualizza e memorizza w-fld a linea w-lin                     *
      *                            posizione w-pos                     *
      *                             ampiezza w-siz                     *
      *----------------------------------------------------------------*
       vis-mef-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *                                                 *
      *              * Se avviene un display effettivo, preventivamen- *
      *              * te si esegue il modo di utilizzo, solo se ne-   *
      *              * cessario                                        *
      *              *-------------------------------------------------*
           if        z-ooo                <    zero
                     if    v-ope          =    "AC"  and
                           v-tip          not  = "T"
                           go to vis-mef-999
                     else  move  high-value
                                          to   z-ool(w-lin)
                           go to vis-mef-500.
           perform   xmx-000              thru xmx-999                .
______*    display   w-fld   bold         line w-lin
           display   w-fld                line w-lin
                                      position w-pos
                                          size w-siz                  .
       vis-mef-500.
      *              *-------------------------------------------------*
      *              * Memorizzazione                                  *
      *              *-------------------------------------------------*
           add       1
                     w-siz              giving w-pnt                  .
           move      w-pnt                to   w-svp                  .
           move      high-value           to   w-chr(w-pnt)           .
           move      w-pos                to   w-pnt                  .
           string    w-fld      delimited by   high-value
                                          into b-lin(w-lin)
                                  with pointer w-pnt                  .
           move      spaces               to   w-chr(w-svp)           .
       vis-mef-999.
           exit.

      *================================================================*
      *    Visualizza e memorizza w-chr(w-ccr) a linea w-lin           *
      *                                      posizione w-pos           *
      *                                       ampiezza 1               *
      *----------------------------------------------------------------*
       vis-mec-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione del carattere, a meno di non es- *
      *              * sere in stato Off                               *
      *              *-------------------------------------------------*
           if        z-ooo                not  < zero
                     move   w-chr(w-ccr)  to   b-chr(w-lin, w-pos)    .
      *              *-------------------------------------------------*
      *              * Esecuzione del modo di utilizzo, solo se ne-    *
      *              * cessario                                        *
      *              *-------------------------------------------------*
           perform   xmx-000              thru xmx-999                .
      *              *-------------------------------------------------*
      *              * Visualizzazione del carattere                   *
      *              *-------------------------------------------------*
           display   b-chr(w-lin, w-pos)  line w-lin
                                      position w-pos                  .
       vis-mec-999.
           exit.

      *================================================================*
      *    Visualizza e memorizza v-txc(w-ccr) a linea w-lin           *
      *                                      posizione w-pos           *
      *                                       ampiezza 1               *
      *----------------------------------------------------------------*
       vis-mct-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione del carattere, a meno di non es- *
      *              * sere in stato Off                               *
      *              *-------------------------------------------------*
           if        z-ooo                not  < zero
                     move   v-txc(w-ccr)  to   b-chr(w-lin, w-pos)    .
      *              *-------------------------------------------------*
      *              * Esecuzione del modo di utilizzo, solo se ne-    *
      *              * cessario                                        *
      *              *-------------------------------------------------*
           perform   xmx-000              thru xmx-999                .
      *              *-------------------------------------------------*
      *              * Visualizzazione del carattere                   *
      *              *-------------------------------------------------*
           display   b-chr(w-lin, w-pos)  line w-lin
                                      position w-pos                  .
       vis-mct-999.
           exit.

      *================================================================*
      * Accettazione di un carattere a linea w-lin position w-pos e    *
      * determinazione dell'eventuale tasto di terminazione usato      *
      *----------------------------------------------------------------*
       acc-key-000.
      *              *-------------------------------------------------*
      *              * Salvataggio carattere attuale per eventuale ri- *
      *              * pristino in caso di Escape 'manuale'            *
      *              *-------------------------------------------------*
           move      b-chr(w-lin, w-pos)  to   w-exs                  .
      *              *-------------------------------------------------*
      *              * Esecuzione del modo di utilizzo, solo se ne-    *
      *              * cessario                                        *
      *              *-------------------------------------------------*
           perform   xmx-000              thru xmx-999                .
      *              *-------------------------------------------------*
      *              * Impostazione carattere                          *
      *              *-------------------------------------------------*
           accept    w-chr(w-ccr)         line w-lin
                                      position w-pos off no beep
                                          on   exception w-exc
                     go to acc-key-100.
      *              *-------------------------------------------------*
      *              * Se non c'e' exception in assoluto               *
      *              *-------------------------------------------------*
           if        w-exc                =    00
                     move  spaces         to   k-key
                     go to acc-key-999.
      *              *-------------------------------------------------*
      *              * Se eccezione non riconosciuta                   *
      *              *-------------------------------------------------*
           move      high-value           to   k-key                  .
           go to     acc-key-999.
       acc-key-100.
      *              *-------------------------------------------------*
      *              * Se c'e' exception                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se l'eccezione corrisponde a Escape la si   *
      *                  * tratta in modo particolare per ovviare ad   *
      *                  * una inconsistenza tipica di Unix            *
      *                  *---------------------------------------------*
           if        w-exc                =    27
                     go to acc-key-400.
      *                  *---------------------------------------------*
      *                  * Determinazione tasto di terminazione        *
      *                  *---------------------------------------------*
           search    all     k-ele 
                     when    k-val(k-inx) =    w-exc
                     move    k-fnc(k-inx) to   k-key
                     go to   acc-key-200.
      *                  *---------------------------------------------*
      *                  * Se non trovato : high-value ed uscita       *
      *                  *---------------------------------------------*
           move      high-value           to   k-key                  .
           go to     acc-key-999.
       acc-key-200.
      *                      *-----------------------------------------*
      *                      * Se Desk-Accessory                       *
      *                      *-----------------------------------------*
       acc-key-201.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        k-key                =    "HELP"
                     go to acc-key-202
           else if   k-key                =    "SHCP"
                     go to acc-key-203
           else      go to acc-key-210.
       acc-key-202.
      *                          *-------------------------------------*
      *                          * Se di tipo HELP                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Desk-Accessory Tipo  *
      *                              * Help                            *
      *                              *---------------------------------*
           perform   dac-hlp-000          thru dac-hlp-999            .
      *                              *---------------------------------*
      *                              * Key a High-Value e uscita       *
      *                              *---------------------------------*
           go to     acc-key-300.
       acc-key-203.
      *                          *-------------------------------------*
      *                          * Se di tipo SHCP                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Desk-Accessory tipo  *
      *                              * Screen-Hard-Copy                *
      *                              *---------------------------------*
           perform   dac-shc-000          thru dac-shc-999            .
      *                              *---------------------------------*
      *                              * Key a High-Value e uscita       *
      *                              *---------------------------------*
           go to     acc-key-300.
       acc-key-210.
      *                      *-----------------------------------------*
      *                      * Se Refresh                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        k-key                not  = "RFSH"
                     go to acc-key-999.
      *                          *-------------------------------------*
      *                          * Esecuzione Refresh                  *
      *                          *-------------------------------------*
           perform   ref-000              thru ref-999                .
      *                          *-------------------------------------*
      *                          * Key a High-Value e uscita           *
      *                          *-------------------------------------*
           go to     acc-key-300.
       acc-key-300.
      *                  *---------------------------------------------*
      *                  * Key a High Value                            *
      *                  *---------------------------------------------*
           move      high-value           to   k-key                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-key-999.
       acc-key-400.
      *              *-------------------------------------------------*
      *              * Trattamento della eccezione di Escape           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione 1. carattere dopo Escape       *
      *                  *---------------------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del 1. carattere       *
      *                  *---------------------------------------------*
           if        k-acc                =    "O"
                     go to acc-key-500
           else if   k-acc                =    "["
                     go to acc-key-600
           else      go to acc-key-300.
       acc-key-500.
      *                  *---------------------------------------------*
      *                  * Se 1. carattere : 'O'                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Accettazione 2. carattere dopo Escape   *
      *                      *-----------------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                      *-----------------------------------------*
      *                      * Test su 2. carattere                    *
      *                      *-----------------------------------------*
           if        k-acc                =    "P"
                     move  "[1] "         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "Q"
                     move  "[2] "         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "R"
                     move  "[3] "         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "S"
                     move  "[4] "         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-600.
      *                  *---------------------------------------------*
      *                  * Se 1. carattere : '['                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Accettazione 2. carattere dopo Escape   *
      *                      *-----------------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                      *-----------------------------------------*
      *                      * Test su 2. carattere                    *
      *                      *-----------------------------------------*
           if        k-acc                =    "A"
                     move  "UP  "         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "B"
                     move  "DOWN"         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "C"
                     move  "RGHT"         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "D"
                     move  "LEFT"         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "1"
                     go to acc-key-650
           else if   k-acc                =    "2"
                     go to acc-key-700
           else if   k-acc                =    "3"
                     go to acc-key-800
           else if   k-acc                =    "4"
                     go to acc-key-850
           else if   k-acc                =    "5"
                     go to acc-key-875
           else if   k-acc                =    "6"
                     go to acc-key-900
           else      go to acc-key-300.
       acc-key-650.
      *                      *-----------------------------------------*
      *                      * Se 2. carattere '1'                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Accettazione 3. carattere dopo E-   *
      *                          * scape                               *
      *                          *-------------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                          *-------------------------------------*
      *                          * Test su 3. carattere                *
      *                          *-------------------------------------*
           if        k-acc                =    "~"
                     move  "FIND"         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "7"
                     go to acc-key-660
           else if   k-acc                =    "8"
                     go to acc-key-670
           else if   k-acc                =    "9"
                     go to acc-key-680
           else      go to acc-key-300.
       acc-key-660.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '7'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "RFSH"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-670.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '8'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "COPY"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-680.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '9'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "PAST"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-700.
      *                      *-----------------------------------------*
      *                      * Se 2. carattere '2'                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Accettazione 3. carattere dopo E-   *
      *                          * scape                               *
      *                          *-------------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                          *-------------------------------------*
      *                          * Test su 3. carattere                *
      *                          *-------------------------------------*
           if        k-acc                =    "~"
                     move  "INSR"         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "0"
                     go to acc-key-705
           else if   k-acc                =    "1"
                     go to acc-key-710
           else if   k-acc                =    "3"
                     go to acc-key-720
           else if   k-acc                =    "4"
                     go to acc-key-725
           else if   k-acc                =    "5"
                     go to acc-key-730
           else if   k-acc                =    "6"
                     go to acc-key-735
           else if   k-acc                =    "8"
                     go to acc-key-745
           else if   k-acc                =    "9"
                     go to acc-key-750
           else      go to acc-key-300.
       acc-key-705.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '0'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "EXPD"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-710.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '1'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "APND"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-720.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '3'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "ICHR"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-725.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '4'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "DCHR"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-730.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '5'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  high-value    to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-735.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '6'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "BACK"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-745.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '8'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "HELP"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-750.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '9'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "DO  "         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-800.
      *                      *-----------------------------------------*
      *                      * Se 2. carattere '3'                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Accettazione 3. carattere dopo E-   *
      *                          * scape                               *
      *                          *-------------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                          *-------------------------------------*
      *                          * Test su 3. carattere                *
      *                          *-------------------------------------*
           if        k-acc                =    "~"
                     move  "REMV"         to   k-key
                     go to acc-key-990
           else if   k-acc                =    "1"
                     go to acc-key-810
           else if   k-acc                =    "2"
                     go to acc-key-815
           else if   k-acc                =    "3"
                     go to acc-key-820
           else if   k-acc                =    "4"
                     go to acc-key-825
           else      go to acc-key-300.
       acc-key-810.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '1'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "DELT"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-815.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '2'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "PRNT"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-820.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '3'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  high-value     to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-825.
      *                          *-------------------------------------*
      *                          * Se 3. carattere '4'                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Accettazione 4. carattere dopo  *
      *                              * Escape                          *
      *                              *---------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                              *---------------------------------*
      *                              * Test su 4. carattere            *
      *                              *---------------------------------*
           if        k-acc                =    "~"
                     move  "EXIT"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-850.
      *                      *-----------------------------------------*
      *                      * Se 2. carattere '4'                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Accettazione 3. carattere dopo E-   *
      *                          * scape                               *
      *                          *-------------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                          *-------------------------------------*
      *                          * Test su 3. carattere                *
      *                          *-------------------------------------*
           if        k-acc                =    "~"
                     move  "SLCT"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-875.
      *                      *-----------------------------------------*
      *                      * Se 2. carattere '5'                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Accettazione 3. carattere dopo E-   *
      *                          * scape                               *
      *                          *-------------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                          *-------------------------------------*
      *                          * Test su 3. carattere                *
      *                          *-------------------------------------*
           if        k-acc                =    "~"
                     move  "PRSC"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-900.
      *                      *-----------------------------------------*
      *                      * Se 2. carattere '6'                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Accettazione 3. carattere dopo E-   *
      *                          * scape                               *
      *                          *-------------------------------------*
           accept    k-acc                line w-lin
                                      position w-pos off no beep      .
      *                          *-------------------------------------*
      *                          * Test su 3. carattere                *
      *                          *-------------------------------------*
           if        k-acc                =    "~"
                     move  "NXSC"         to   k-key
                     go to acc-key-990
           else      go to acc-key-300.
       acc-key-990.
      *              *-------------------------------------------------*
      *              * Esecuzione del modo di utilizzo, solo se ne-    *
      *              * cessario                                        *
      *              *-------------------------------------------------*
           perform   xmx-000              thru xmx-999                .
      *              *-------------------------------------------------*
      *              * Rivisualizzazione carattere salvato in caso di  *
      *              * terminazione per mezzo di Escape 'manuale'      *
      *              *-------------------------------------------------*
           display   w-exs                line w-lin
                                      position w-pos                  .
       acc-key-999.
           exit.

      *================================================================*
      *    Test se function-key prevista in impostazione               *
      *----------------------------------------------------------------*
       tst-pfk-000.
      *              *-------------------------------------------------*
      *              * Test se function-key sempre ammessa             *
      *              *-------------------------------------------------*
           set       k-tfx                to   1                      .
           search    k-tfu
                     at end go to tst-pfk-200
                     when   k-tfu
                           (k-tfx)        =    k-key
                            go to tst-pfk-999.
       tst-pfk-200.
      *              *-------------------------------------------------*
      *              * Test se function-key esplicitamente prevista    *
      *              *-------------------------------------------------*
           move      zero                 to   w-pnt                  .
       tst-pfk-400.
           if        w-pnt                <    40
                     add   1              to   w-pnt
                     if    v-pfk(w-pnt)   =    k-key
                           go to tst-pfk-999
                     else  go to tst-pfk-400.
      *              *-------------------------------------------------*
      *              * Se function-key non prevista                    *
      *              *-------------------------------------------------*
           move      high-values          to   k-key                  .
       tst-pfk-999.
           exit.

      *================================================================*
      *    Determinazione caratteristiche per editing numerico         *
      *----------------------------------------------------------------*
       car-edn-000.
      *              *-------------------------------------------------*
      *              * Se raggruppamento a tre a tre                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "G"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-rag
           else      move   "G"           to   w-rag                  .
      *              *-------------------------------------------------*
      *              * Se zeri in testa                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "9"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-zit
           else      move   "9"           to   w-zit                  .
      *              *-------------------------------------------------*
      *              * Se blank when zero                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "B"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-bwz
           else      move   "B"           to   w-bwz                  .
      *              *-------------------------------------------------*
      *              * Se decimali quanti sono                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "D"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-dqs
           else      move   "D"           to   w-dqs                  .
      *              *-------------------------------------------------*
      *              * Se no editing finale                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "N"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-noe
           else      move   "N"           to   w-noe                  .
      *              *-------------------------------------------------*
      *              * Se allineamento a sinistra                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-asx                  .
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "<"                                   .
           if        w-ctr                >    zero   and
                     w-zit                =    spaces
                     move   "<"           to   w-asx                  .
      *              *-------------------------------------------------*
      *              * Se maschera di editing                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
           inspect   v-edm            tallying w-ctr
                     for    all "M"                                   .
           if        w-ctr                =    zero
                     move   spaces        to   w-msk
           else      move   "M"           to   w-msk                  .
      *              *-------------------------------------------------*
      *              * Determinazione lunghezza totale campo editato   *
      *              * in v-edl, e dell' indice per unstring in w-inx  *
      *              *-------------------------------------------------*
           if        w-msk                =    spaces
                     go to car-edn-500.
      *                     *------------------------------------------*
      *                     * Se editing con maschera                  *
      *                     *------------------------------------------*
           move      zero                 to   v-edl                  .
           inspect   v-msk            tallying v-edl
                     for    characters  before initial space          .
           move      1                    to   w-inx                  .
           go to     car-edn-999.
       car-edn-500.
      *                     *------------------------------------------*
      *                     * Se editing senza maschera                *
      *                     *------------------------------------------*
           move      v-car                to   v-edl                  .
           subtract  v-car                from 19
                                        giving w-inx                  .
           if        v-dec                >    zero
                     add      v-dec       to   v-edl
                     add      1           to   v-edl                  .
           if        v-sgn                =    "S"
                     add      1           to   v-edl
                     subtract 1           from w-inx                  .
           if        w-rag                not  = spaces
                     move     v-car       to   w-ctr
                     subtract 1           from w-ctr
                     divide   3           into w-ctr
                     add      w-ctr       to   v-edl
                     subtract w-ctr       from w-inx                  .
       car-edn-999.
           exit.

      *================================================================*
      *    Editing campo tipo numerico w-num in v-edt lungo v-edl      *
      *----------------------------------------------------------------*
       num-edt-000.
      *              *-------------------------------------------------*
      *              * Preparazione area editata a spaces              *
      *              *-------------------------------------------------*
           move      spaces               to   w-ned                  .
      *              *-------------------------------------------------*
      *              * Test se campo = zero e clausola blank when zero *
      *              *-------------------------------------------------*
           if        w-num                =    zero and
                     w-bwz                not  = spaces
                     go to num-edt-700.
      *              *-------------------------------------------------*
      *              * Test se editing con maschera o normale          *
      *              *-------------------------------------------------*
           if        w-msk                =    spaces
                     go to num-edt-400.
      *              *-------------------------------------------------*
      *              * Editing con maschera                            *
      *              *-------------------------------------------------*
           move      w-num                to   w-nem                  .
           move      v-msk                to   w-ned                  .
           move      v-edl                to   w-inx                  .
           move      13                   to   w-pnt                  .
       num-edt-200.
           if        w-ned-chr(w-inx)     =    "X"
                     move  w-nex(w-pnt)   to   w-ned-chr(w-inx)
                     subtract 1           from w-pnt
           else      if  w-ned-chr(w-inx) =    "b"
                         move  spaces     to   w-ned-chr(w-inx)       .
           if        w-inx                >    1
                     subtract 1           from w-inx
                     go to num-edt-200
           else      go to num-edt-700.
       num-edt-400.
      *              *-------------------------------------------------*
      *              * Editing senza maschera                          *
      *              *-------------------------------------------------*
      *                     *------------------------------------------*
      *                     * Editing preliminare                      *
      *                     *------------------------------------------*
           if        w-rag                =    spaces
                     if     w-zit         =    spaces
                            move   w-num  to   w-ned-ns9
                     else   move   w-num  to   w-ned-nc9
           else      if     w-zit         =    spaces
                            move   w-num  to   w-ned-rs9
                     else   move   w-num  to   w-ned-rc9              .
      *                     *------------------------------------------*
      *                     * Posizionamento segno se presente         *
      *                     *------------------------------------------*
           if        w-num                <    zero and
                     v-sgn                not  = spaces
                     if    w-zit          not  = spaces
                           move  "-"      to   w-ned-chr(w-inx)
                     else  move    zero   to   w-ctr
                           inspect w-ned  tallying w-ctr
                                          for  leading spaces
                           move    "-"    to   w-ned-chr(w-ctr)       .
      *                     *------------------------------------------*
      *                     * Test se clausola di editing "D"          *
      *                     *------------------------------------------*
           if        w-dqs                =    spaces or
                     v-dec                not  > zero
                     go to num-edt-700.
           move      24                   to   w-ctr                  .
       num-edt-500.
           if        w-ned-chr(w-ctr)     =    ","
                     move     spaces      to   w-ned-chr(w-ctr)
                     go to    num-edt-700.
           if        w-ned-chr(w-ctr)     =    "0"
                     move     spaces      to   w-ned-chr(w-ctr)
                     subtract 1           from w-ctr
                     go to    num-edt-500.
       num-edt-700.
      *                     *------------------------------------------*
      *                     * Trasferimento campo editato in uscita    *
      *                     *------------------------------------------*
           move      spaces               to   v-edt                  .
           add       w-inx
                     v-edl              giving w-pnt                  .
           move      high-value           to   w-ned-chr(w-pnt)       .
      *                         *--------------------------------------*
      *                         * Test se allineamento a sinistra      *
      *                         *--------------------------------------*
           if        w-asx                =    spaces
                     move  w-inx          to   w-pnt
                     go to num-edt-800.
           move      zero                 to   w-pnt                  .
           inspect   w-ned            tallying w-pnt
                                   for leading spaces                 .
           add       1                    to   w-pnt                  .
       num-edt-800.
           unstring  w-ned      delimited by   high-value
                                          into v-edt
                                  with pointer w-pnt                  .
       num-edt-999.
           exit.

      *================================================================*
      *    Inversione campo tipo data da w-amg a w-dat                 *
      *----------------------------------------------------------------*
       dat-inv-000.
           move      w-ann                to   w-aaa                  .
           move      w-mes                to   w-mmm                  .
           move      w-gio                to   w-ggg                  .
       dat-inv-999.
           exit.
           
      *================================================================*
      *    Editing campo tipo data w-dat in v-edl lungo 8              *
      *----------------------------------------------------------------*
       dat-edt-000.
           move      spaces               to   v-edt                  .
           move      w-dat                to   w-ded                  .
           move      w-dte                to   v-edt                  .
           move      8                    to   v-edl                  .
       dat-edt-999.
           exit.
           
      *================================================================*
      *    Uppercase campo in w-fld lungo w-siz                        *
      *----------------------------------------------------------------*
       upp-cas-000.
           move      zero                 to   w-ccr                  .
       upp-cas-200.
           add       1                    to   w-ccr                  .
           if        w-ccr                >    w-siz
                     go to upp-cas-999.
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-chr(w-ccr)   .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-chr(w-ccr)           .
           go to     upp-cas-200.
       upp-cas-999.
           exit.

      *================================================================*
      *    Lowercase campo in w-fld lungo w-siz                        *
      *----------------------------------------------------------------*
       low-cas-000.
           move      zero                 to   w-ccr                  .
       low-cas-200.
           add       1                    to   w-ccr                  .
           if        w-ccr                >    w-siz
                     go to low-cas-999.
           move      zero                 to   w-ulc                  .
           inspect   w-upp            tallying w-ulc
                     for characters     before initial w-chr(w-ccr)   .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-loc(w-ulc) to   w-chr(w-ccr)           .
           go to     low-cas-200.
       low-cas-999.
           exit.

      *    *===========================================================*
      *    * Estrazione system date                                    *
      *    *-----------------------------------------------------------*
       est-sdt-000.
      *              *-------------------------------------------------*
      *              * Accettazione system date and time               *
      *              *-------------------------------------------------*
           move      "DT"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Data in area di destinazione                    *
      *              *-------------------------------------------------*
           move      o-dat                to   i-dat                  .
       est-sdt-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione secolo nella data                         *
      *    *-----------------------------------------------------------*
       nor-sec-000.
           if        i-dtc                =    zero
                     go to nor-sec-999.
           move      "NS"                 to   o-ope                  .
           move      i-dat                to   o-dat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-dat                to   i-dat                  .
       nor-sec-999.
           exit.

      *    *===========================================================*
      *    * Determinazione (data) + (nr. giorni) = (data)             *
      *    *                                                           *
      *    * N.B.: subroutine personalizzata per driver video          *
      *    *-----------------------------------------------------------*
       det-dat-nrg-000.
      *              *-------------------------------------------------*
      *              * Data base in data in output                     *
      *              *-------------------------------------------------*
           move      w-det-dat-nrg-dtb    to   w-det-dat-nrg-dti      .
      *              *-------------------------------------------------*
      *              * Se numero giorni di incremento a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-det-dat-nrg-ngi    =    zero
                     go to det-dat-nrg-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di primo passaggio         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-dat-nrg-fpp      .
      *              *-------------------------------------------------*
      *              * Inizializzazione progressivo giorni utilizzati  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-dat-nrg-pgu      .
       det-dat-nrg-100.
      *              *-------------------------------------------------*
      *              * Ciclo di determinazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione data di fine mese            *
      *                  *---------------------------------------------*
           move      w-det-dat-nrg-dti    to   w-det-nrg-dat-dat      .
           move      31                   to   w-det-nrg-dat-gio      .
       det-dat-nrg-110.
      *                          *-------------------------------------*
      *                          * Controllo della data                *
      *                          *-------------------------------------*
           perform   cnt-dat-000          thru cnt-dat-999            .
      *                          *-------------------------------------*
      *                          * Test su esito controllo             *
      *                          *-------------------------------------*
           if        w-det-nrg-dat-sts    not  = spaces
                     subtract 1           from w-det-nrg-dat-gio
                     go to det-dat-nrg-110.
      *                  *---------------------------------------------*
      *                  * Aggiornamento data incrementata             *
      *                  *---------------------------------------------*
           move      w-det-nrg-dat-dat    to   w-det-dat-nrg-dti      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni utilizzati     *
      *                  *---------------------------------------------*
       det-dat-nrg-120.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che sia il primo   *
      *                      * passaggio o no                          *
      *                      *-----------------------------------------*
           if        w-det-dat-nrg-fpp    =    spaces
                     go to det-dat-nrg-122
           else      go to det-dat-nrg-124.
       det-dat-nrg-122.
      *                      *-----------------------------------------*
      *                      * Primo passaggio                         *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-dat-nrg-fpp      .
           subtract  w-det-dat-nrg-dbg    from w-det-dat-nrg-dig
                                        giving w-det-dat-nrg-ngu      .
           go to     det-dat-nrg-130.
       det-dat-nrg-124.
      *                      *-----------------------------------------*
      *                      * Passaggio successivo al primo           *
      *                      *-----------------------------------------*
           move      w-det-dat-nrg-dig    to   w-det-dat-nrg-ngu      .
           go to     det-dat-nrg-130.
       det-dat-nrg-130.
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivo giorni utilizzati *
      *                  *---------------------------------------------*
           add       w-det-dat-nrg-ngu    to   w-det-dat-nrg-pgu      .
      *                  *---------------------------------------------*
      *                  * Se progressivo giorni utilizzati superiore  *
      *                  * al numero giorni di incremento richiesti :  *
      *                  * a fine ciclo                                *
      *                  *---------------------------------------------*
           if        w-det-dat-nrg-pgu    not  < w-det-dat-nrg-ngi
                     go to det-dat-nrg-150.
      *                  *---------------------------------------------*
      *                  * Incremento mese/anno                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-dat-nrg-dim      .
           if        w-det-dat-nrg-dim    >    12
                     move  1              to   w-det-dat-nrg-dim
                     add   1              to   w-det-dat-nrg-dia      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-dat-nrg-100.
       det-dat-nrg-150.
      *              *-------------------------------------------------*
      *              * Fine ciclo                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni in esubero ri- *
      *                  * spetto a quelli richiesti                   *
      *                  *---------------------------------------------*
           subtract  w-det-dat-nrg-ngi    from w-det-dat-nrg-pgu
                                        giving w-det-dat-nrg-ngu      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento giorno della data incrementa- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           subtract  w-det-dat-nrg-ngu    from w-det-dat-nrg-dig      .
       det-dat-nrg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione (data) - (nr. giorni) = (data)             *
      *    *                                                           *
      *    * N.B.: subroutine personalizzata per driver video          *
      *    *-----------------------------------------------------------*
       det-nrg-dat-000.
      *              *-------------------------------------------------*
      *              * Data base in data in output                     *
      *              *-------------------------------------------------*
           move      w-det-nrg-dat-dtb    to   w-det-nrg-dat-dtd      .
      *              *-------------------------------------------------*
      *              * Se numero giorni di decremento a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-det-nrg-dat-ngd    =    zero
                     go to det-nrg-dat-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di primo passaggio         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-nrg-dat-fpp      .
      *              *-------------------------------------------------*
      *              * Inizializzazione progressivo giorni utilizzati  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nrg-dat-pgu      .
       det-nrg-dat-100.
      *              *-------------------------------------------------*
      *              * Ciclo di determinazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura data di inizio mese               *
      *                  *---------------------------------------------*
           move      01                   to   w-det-nrg-dat-ddg      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni utilizzati     *
      *                  *---------------------------------------------*
       det-nrg-dat-120.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che sia il primo   *
      *                      * passaggio o no                          *
      *                      *-----------------------------------------*
           if        w-det-nrg-dat-fpp    =    spaces
                     go to det-nrg-dat-122
           else      go to det-nrg-dat-124.
       det-nrg-dat-122.
      *                      *-----------------------------------------*
      *                      * Primo passaggio                         *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-nrg-dat-fpp      .
           subtract  w-det-nrg-dat-ddg    from w-det-nrg-dat-dbg
                                        giving w-det-nrg-dat-ngu      .
           go to     det-nrg-dat-130.
       det-nrg-dat-124.
      *                      *-----------------------------------------*
      *                      * Passaggio successivo al primo           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione numero giorni del    *
      *                          * mese                                *
      *                          *-------------------------------------*
           move      w-det-nrg-dat-dtd    to   w-det-nrg-dat-dat      .
           move      31                   to   w-det-nrg-dat-gio      .
       det-nrg-dat-125.
      *                          *-------------------------------------*
      *                          * Controllo della data                *
      *                          *-------------------------------------*
           perform   cnt-dat-000          thru cnt-dat-999            .
      *                          *-------------------------------------*
      *                          * Test su esito controllo             *
      *                          *-------------------------------------*
           if        w-det-nrg-dat-sts    not  = spaces
                     subtract 1           from w-det-nrg-dat-gio
                     go to det-nrg-dat-125.
           move      w-det-nrg-dat-gio    to   w-det-nrg-dat-ngu      .
           go to     det-nrg-dat-130.
       det-nrg-dat-130.
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivo giorni utilizzati *
      *                  *---------------------------------------------*
           add       w-det-nrg-dat-ngu    to   w-det-nrg-dat-pgu      .
      *                  *---------------------------------------------*
      *                  * Se progressivo giorni utilizzati superiore  *
      *                  * al numero giorni di decremento richiesti :  *
      *                  * a fine ciclo                                *
      *                  *---------------------------------------------*
           if        w-det-nrg-dat-pgu    not  < w-det-nrg-dat-ngd
                     go to det-nrg-dat-150.
      *                  *---------------------------------------------*
      *                  * Decremento mese/anno                        *
      *                  *---------------------------------------------*
           subtract  1                    from w-det-nrg-dat-ddm      .
           if        w-det-nrg-dat-ddm    =    zero
                     move  12             to   w-det-nrg-dat-ddm
                     subtract 1           from w-det-nrg-dat-dda      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-nrg-dat-100.
       det-nrg-dat-150.
      *              *-------------------------------------------------*
      *              * Fine ciclo                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni in esubero ri- *
      *                  * spetto a quelli richiesti                   *
      *                  *---------------------------------------------*
           subtract  w-det-nrg-dat-ngd    from w-det-nrg-dat-pgu
                                        giving w-det-nrg-dat-ngu      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento giorno della data decrementa- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           add       w-det-nrg-dat-ngu    to   w-det-nrg-dat-ddg      .
       det-nrg-dat-999.
           exit.

      *    *===========================================================*
      *    * Controllo data                                            *
      *    *                                                           *
      *    * N.B.: subroutine proveniente dalla segreteria             *
      *    *-----------------------------------------------------------*
       cnt-dat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-nrg-dat-sts      .
      *              *-------------------------------------------------*
      *              * Se data a zero : uscita                         *
      *              *-------------------------------------------------*
           if        w-det-nrg-dat-dat    =    zero
                     go to cnt-dat-999.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari per errori nella data     *
      *              *-------------------------------------------------*
           if        w-det-nrg-dat-sec    >    1    or
                     w-det-nrg-dat-mes    =    zero or
                     w-det-nrg-dat-mes    >    12   or
                     w-det-nrg-dat-gio    =    zero or
                     w-det-nrg-dat-gio    >    31
                     go to cnt-dat-900.
      *              *-------------------------------------------------*
      *              * Test per mese 2 - febbraio                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo per l'anno bisestile                *
      *                  *---------------------------------------------*
           if        w-det-nrg-dat-mes    =    02
                     divide 4             into w-det-nrg-dat-ann
                                        giving w-ctr
                                     remainder w-pnt
      *                  *---------------------------------------------*
      *                  * Se la divisione per 4 ha restituito zero    *
      *                  *---------------------------------------------*
                     if   w-pnt           =    zero
      *                      *-----------------------------------------*
      *                      * Se l'anno e' zero                       *
      *                      *-----------------------------------------*
                          if   w-det-nrg-dat-ann
                                          =    zero
      *                          *-------------------------------------*
      *                          * Se il secolo e' zero                *
      *                          *-------------------------------------*
                               if   w-det-nrg-dat-sec
                                          =    zero
                                    move 28
                                          to   w-tgp (w-det-nrg-dat-mes)
                               else move 29
                                          to   w-tgp
                                              (w-det-nrg-dat-mes)
                          else move 29    to   w-tgp
                                              (w-det-nrg-dat-mes)
                     else move 28         to   w-tgp
                                              (w-det-nrg-dat-mes)     .
      *              *-------------------------------------------------*
      *              * Test finale                                     *
      *              *-------------------------------------------------*
           if        w-det-nrg-dat-gio    not  > w-tgp
                                                (w-det-nrg-dat-mes)
                     go to cnt-dat-999.
       cnt-dat-900.
      *              *-------------------------------------------------*
      *              * Flag di errore in uscita                        *
      *              *-------------------------------------------------*
           move      "##"                 to   w-det-nrg-dat-sts      .
       cnt-dat-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione del giorno della settimana      *
      *    *-----------------------------------------------------------*
      *    * In input  : w-det-dow-dat = Data completa                 *
      *    *                                                           *
      *    * In output : w-det-dow-lit = Giorno della settimana, lite- *
      *    *                             ral                           *
      *    *                                                           *
      *    *             w-det-dow-num = Giorno della settimana, nu-   *
      *    *                             mero secondo la tabella :     *
      *    *                                                           *
      *    *                             -  1 : Lunedi'                *
      *    *                             -  2 : Martedi'               *
      *    *                             -  3 : Mercoledi'             *
      *    *                             -  4 : Giovedi'               *
      *    *                             -  5 : Venerdi'               *
      *    *                             -  6 : Sabato                 *
      *    *                             -  7 : Domenica               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wgdslit0.cps"                   .
