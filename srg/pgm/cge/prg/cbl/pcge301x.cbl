       Identification Division.
       Program-Id.                                 pcge301x           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    mov                 *
      *                                   Fase:    cge301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 15/03/90    *
      *                       Ultima revisione:    NdK del 20/03/18    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Expand registrazione                        *
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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Record file                                               *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [mgi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgi"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Multinumerazione giornale iva vendite                 *
      *        *-------------------------------------------------------*
           05  w-prs-mul-giv.
      *            *---------------------------------------------------*
      *            * Si/No multinumerazione                            *
      *            *---------------------------------------------------*
               10  w-prs-mul-giv-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Max codice per multinumerazione                   *
      *            *---------------------------------------------------*
               10  w-prs-mul-giv-max      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Multinumerazione giornale iva acquisti                *
      *        *-------------------------------------------------------*
           05  w-prs-mul-gia.
      *            *---------------------------------------------------*
      *            * Si/No multinumerazione                            *
      *            *---------------------------------------------------*
               10  w-prs-mul-gia-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Max codice per multinumerazione                   *
      *            *---------------------------------------------------*
               10  w-prs-mul-gia-max      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Multinumerazione giornale iva corrispettivi           *
      *        *-------------------------------------------------------*
           05  w-prs-mul-gic.
      *            *---------------------------------------------------*
      *            * Si/No multinumerazione                            *
      *            *---------------------------------------------------*
               10  w-prs-mul-gic-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Max codice per multinumerazione                   *
      *            *---------------------------------------------------*
               10  w-prs-mul-gic-max      pic  9(02)                  .

      *    *===========================================================*
      *    * Work area per lettura files                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Per file [mgt]                                        *
      *        *-------------------------------------------------------*
           05  w-let-fil-mgt.
               10  w-let-fil-mgt-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Per file [mgr]                                        *
      *        *-------------------------------------------------------*
           05  w-let-fil-mgr.
               10  w-let-fil-mgr-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Per file [mgi]                                        *
      *        *-------------------------------------------------------*
           05  w-let-fil-mgi.
               10  w-let-fil-mgi-flg      pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per visualizzazione espansione registrazione    *
      *    *-----------------------------------------------------------*
       01  w-vis-exp-reg.
           05  w-vis-exp-reg-ctr          pic  9(02)                  .
           05  w-vis-exp-reg-lin          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
           05  w-mpn-dat-reg              pic  9(07)                  .
           05  w-mpn-num-prt              pic  9(07)                  .
           05  w-mpn-num-prg              pic  9(05)                  .

      ******************************************************************
       Procedure Division                using w-mpn                  .
      ******************************************************************

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione relativa alla multi-  *
      *              * numerazione giornale iva vendite                *
      *              *-------------------------------------------------*
           perform   prs-mul-giv-000      thru prs-mul-giv-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione relativa alla multi-  *
      *              * numerazione giornale iva acquisti               *
      *              *-------------------------------------------------*
           perform   prs-mul-gia-000      thru prs-mul-gia-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione relativa alla multi-  *
      *              * numerazione giornale iva corrispettivi          *
      *              *-------------------------------------------------*
           perform   prs-mul-gic-000      thru prs-mul-gic-999        .
      *              *-------------------------------------------------*
      *              * Lettura [mgt]                                   *
      *              *-------------------------------------------------*
           perform   let-fil-mgt-000      thru let-fil-mgt-999        .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
           if        w-let-fil-mgt-flg    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Lettura [mgr]                                   *
      *              *-------------------------------------------------*
           perform   let-fil-mgr-000      thru let-fil-mgr-999        .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
           if        w-let-fil-mgr-flg    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Lettura [mgi]                                   *
      *              *-------------------------------------------------*
           perform   let-fil-mgi-000      thru let-fil-mgi-999        .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
           if        w-let-fil-mgi-flg    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione espansione registrazione        *
      *              *-------------------------------------------------*
           perform   vis-exp-reg-000      thru vis-exp-reg-999        .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative alla multinume-  *
      *    * razione del giornale iva vendite                          *
      *    *-----------------------------------------------------------*
       prs-mul-giv-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge/iva[mul-giv]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-mul-giv
           else      move  spaces         to   w-prs-mul-giv          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-mul-giv-snx    not  = "S"
                     move  "N"            to   w-prs-mul-giv-snx      .
           if        w-prs-mul-giv-snx    not  = "S"
                     move  00             to   w-prs-mul-giv-max      .
           if        w-prs-mul-giv-max    not  numeric or
                     w-prs-mul-giv-max    >    29
                     move  29             to   w-prs-mul-giv-max      .
       prs-mul-giv-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative alla multinume-  *
      *    * razione del giornale iva acquisti                         *
      *    *-----------------------------------------------------------*
       prs-mul-gia-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge/iva[mul-gia]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-mul-gia
           else      move  spaces         to   w-prs-mul-gia          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-mul-gia-snx    not  = "S"
                     move  "N"            to   w-prs-mul-gia-snx      .
           if        w-prs-mul-gia-snx    not  = "S"
                     move  00             to   w-prs-mul-gia-max      .
           if        w-prs-mul-gia-max    not  numeric or
                     w-prs-mul-gia-max    >    29
                     move  29             to   w-prs-mul-gia-max      .
       prs-mul-gia-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative alla multinume-  *
      *    * razione del giornale iva corrispettivi                    *
      *    *-----------------------------------------------------------*
       prs-mul-gic-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge/iva[mul-gic]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-mul-gic
           else      move  spaces         to   w-prs-mul-gic          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-mul-gic-snx    not  = "S"
                     move  "N"            to   w-prs-mul-gic-snx      .
           if        w-prs-mul-gic-snx    not  = "S"
                     move  00             to   w-prs-mul-gic-max      .
           if        w-prs-mul-gic-max    not  numeric or
                     w-prs-mul-gic-max    >    29
                     move  29             to   w-prs-mul-gic-max      .
       prs-mul-gic-999.
           exit.

      *    *===========================================================*
      *    * Lettura [mgt]                                             *
      *    *-----------------------------------------------------------*
       let-fil-mgt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-fil-mgt-flg      .
      *              *-------------------------------------------------*
      *              * Apertura file [mgt]                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * Lettura file [mgt]                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG"             to   f-key                  .
           move      w-mpn-dat-reg        to   rf-mgt-dat-reg         .
           move      w-mpn-num-prt        to   rf-mgt-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * Se errore : set del flag                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-let-fil-mgt-flg      .
      *              *-------------------------------------------------*
      *              * Chiusura file [mgt]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
       let-fil-mgt-999.
           exit.

      *    *===========================================================*
      *    * Lettura [mgr]                                             *
      *    *-----------------------------------------------------------*
       let-fil-mgr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-fil-mgr-flg      .
      *              *-------------------------------------------------*
      *              * Apertura file [mgr]                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Lettura file [mgr]                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG"             to   f-key                  .
           move      w-mpn-dat-reg        to   rf-mgr-dat-reg         .
           move      w-mpn-num-prt        to   rf-mgr-num-prt         .
           move      w-mpn-num-prg        to   rf-mgr-num-prg         .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Se errore : set del flag                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-let-fil-mgr-flg      .
      *              *-------------------------------------------------*
      *              * Chiusura file [mgr]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
       let-fil-mgr-999.
           exit.

      *    *===========================================================*
      *    * Lettura [mgi]                                             *
      *    *-----------------------------------------------------------*
       let-fil-mgi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-fil-mgi-flg      .
      *              *-------------------------------------------------*
      *              * Test sul tipo di movimento Iva                  *
      *              *                                                 *
      *              * Solo Dare / Avere Fornitore italia e estero CEE *
      *              *-------------------------------------------------*
           if        rf-mgt-tip-iva       not  = "4"   and
                     rf-mgt-tip-iva       not  = "5"   and
                     rf-mgt-tip-iva       not  = "D"   and
                     rf-mgt-tip-iva       not  = "E"
                     go to let-fil-mgi-999.
      *              *-------------------------------------------------*
      *              * Apertura file [mgi]                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *              *-------------------------------------------------*
      *              * Lettura file [mgi]                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATPRT"             to   f-key                  .
           move      w-mpn-dat-reg        to   rf-mgi-dat-reg         .
           move      w-mpn-num-prt        to   rf-mgi-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *              *-------------------------------------------------*
      *              * Se errore : set del flag                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-let-fil-mgi-flg      .
      *              *-------------------------------------------------*
      *              * Chiusura file [mgi]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
       let-fil-mgi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione espansione registrazione                  *
      *    *-----------------------------------------------------------*
       vis-exp-reg-000.
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
      *              * Visualizzazione box internamente vuoto          *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione registrazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione operazione                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      "Descrizione operazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valori                                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-vis-exp-reg-ctr      .
           move      07                   to   w-vis-exp-reg-lin      .
       vis-exp-reg-100.
           add       1                    to   w-vis-exp-reg-ctr      .
           if        w-vis-exp-reg-ctr    >    4
                     go to vis-exp-reg-200.
           if        rf-mgt-rig-cau
                    (w-vis-exp-reg-ctr)   =    spaces
                     go to vis-exp-reg-100.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-vis-exp-reg-lin    to   v-lin                  .
           move      04                   to   v-pos                  .
           move      rf-mgt-rig-cau
                    (w-vis-exp-reg-ctr)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           add       1                    to   w-vis-exp-reg-lin      .
           go to     vis-exp-reg-100.
       vis-exp-reg-200.
      *                  *---------------------------------------------*
      *                  * Commento                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      "Commento :"         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      rf-mgr-com-rig       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Archivio                                    *
      *                  *---------------------------------------------*
           if        rf-mgr-tip-arc       =    "C"
                     go to vis-exp-reg-250
           else if   rf-mgr-tip-arc       =    "F"
                     go to vis-exp-reg-300
           else      go to vis-exp-reg-350.
       vis-exp-reg-250.
      *                      *-----------------------------------------*
      *                      * Archivio 'C'                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      "Cliente    :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Lettura archivio                    *
      *                          *-------------------------------------*
           perform   let-cod-cli-000      thru let-cod-cli-999        .
      *                          *-------------------------------------*
      *                          * Codice                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      17                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      spaces               to   v-edm                  .
           move      rf-mgr-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .

      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      rf-cli-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-exp-reg-350.
       vis-exp-reg-300.
      *                      *-----------------------------------------*
      *                      * Archivio 'F'                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      "Fornitore  :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Lettura archivio                    *
      *                          *-------------------------------------*
           perform   let-cod-fnt-000      thru let-cod-fnt-999        .
      *                          *-------------------------------------*
      *                          * Codice                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      17                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      spaces               to   v-edm                  .
           move      rf-mgr-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .

      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      rf-fnt-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     vis-exp-reg-350.
       vis-exp-reg-350.
      *                      *-----------------------------------------*
      *                      * Archivio 'G'                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      "Sottoconto :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Lettura archivio                    *
      *                          *-------------------------------------*
           perform   let-cod-pdc-000      thru let-cod-pdc-999        .
      *                          *-------------------------------------*
      *                          * Codice                              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing con appoggio a destra   *
      *                              *---------------------------------*
           if        rf-mgr-cod-pdc       >    99999
                     move   3             to   w-edt-cod-pdc-liv
           else      move   2             to   w-edt-cod-pdc-liv      .
           move      rf-mgr-cod-pdc       to   w-edt-cod-pdc-cod      .
           move      spaces               to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-adx-000      thru edt-pdc-adx-999        .
      *                              *---------------------------------*
      *                              * Visualizzazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Descrizione sottoconto              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      rf-pdc-des-pdc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-exp-reg-400.
      *                  *---------------------------------------------*
      *                  * Immissione                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      "Immissione :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valori                                  *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-mgt-ide-dat       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    "["        delimited by   size
                     v-edt      delimited by   spaces
                     "] ["      delimited by   size
                     rf-mgt-ide-ute       
                                delimited by   spaces
                     "] ["      delimited by   size
                     rf-mgt-ide-fas       
                                delimited by   spaces
                     "]"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Movimento di bilancio                       *
      *                  *---------------------------------------------*
           if        rf-mgt-snx-mob       =    "N"
                     go to vis-exp-reg-500.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      "* Movimento di bilancio *"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-exp-reg-500.
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Data registrazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      05                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      rf-mgr-dat-reg       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Nr.  protocollo    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      06                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      rf-mgr-num-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice causale                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Codice causale     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      07                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      rf-mgt-cod-cau       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Eventuale indicatore di Reverse Charge      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su flag di Reverse Charge          *
      *                      *-----------------------------------------*
           if        rf-mgi-flg-rfp       not  = "R"
                     go to vis-exp-reg-550.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      "{R}"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-exp-reg-550.
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Data documento     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      09                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      rf-mgr-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Nr.  documento     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      rf-mgr-num-doc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data riferimento                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Data riferimento   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      rf-mgr-dat-rif       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero riferimento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Nr.  riferimento   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      rf-mgr-num-rif       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-exp-reg-600.
      *                  *---------------------------------------------*
      *                  * Numero protocollo iva                       *
      *                  *---------------------------------------------*
           if        rf-mgt-tip-iva       not  = "4" and
                     rf-mgt-tip-iva       not  = "5" and
                     rf-mgt-tip-iva       not  = "6" and
                     rf-mgt-tip-iva       not  = "D" and
                     rf-mgt-tip-iva       not  = "E"
                     go to vis-exp-reg-700.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Protocollo iva     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      15                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      rf-mgt-prt-iva       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-exp-reg-700.
      *                  *---------------------------------------------*
      *                  * Numero giornale iva                         *
      *                  *---------------------------------------------*
           if        rf-mgt-tip-iva       =    "1" or
                     rf-mgt-tip-iva       =    "2" or
                     rf-mgt-tip-iva       =    "A" or
                     rf-mgt-tip-iva       =    "B"
                     go to vis-exp-reg-710
           else if   rf-mgt-tip-iva       =    "4" or
                     rf-mgt-tip-iva       =    "5" or
                     rf-mgt-tip-iva       =    "6" or
                     rf-mgt-tip-iva       =    "D" or
                     rf-mgt-tip-iva       =    "E"
                     go to vis-exp-reg-720
           else if   rf-mgt-tip-iva       =    "3"
                     go to vis-exp-reg-730
           else      go to vis-exp-reg-800.
       vis-exp-reg-710.
           if        w-prs-mul-giv-snx    =    "S" and
                     w-prs-mul-giv-max    >    zero
                     go to vis-exp-reg-750
           else      go to vis-exp-reg-800.
       vis-exp-reg-720.
           if        w-prs-mul-gia-snx    =    "S" and
                     w-prs-mul-gia-max    >    zero
                     go to vis-exp-reg-750
           else      go to vis-exp-reg-800.
       vis-exp-reg-730.
           if        w-prs-mul-gic-snx    =    "S" and
                     w-prs-mul-gic-max    >    zero
                     go to vis-exp-reg-750
           else      go to vis-exp-reg-800.
       vis-exp-reg-750.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      47                   to   v-pos                  .
           move      "Nr. giornale iva   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      68                   to   v-pos                  .
           move      rf-mgt-cod-num       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-exp-reg-800.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-exp-reg-900.
      *              *-------------------------------------------------*
      *              * Accettazione di un carattere per presa visione  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (1)              .
           move      "DO  "               to   v-pfk (2)              .
           move      "DOWN"               to   v-pfk (3)              .
           move      "UP  "               to   v-pfk (4)              .
           move      "EXPD"               to   v-pfk (5)              .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-exp-reg-999.
           exit.

      *    *===========================================================*
      *    * Lettura [cli]                                             *
      *    *-----------------------------------------------------------*
       let-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Se codice a zero                                *
      *              *-------------------------------------------------*
           if        rf-mgr-cod-arc       =    zero
                     move  spaces         to   rf-cli-rag-soc
                     go to let-cod-cli-999.
      *              *-------------------------------------------------*
      *              * Apertura file [cli]                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Lettura file [cli]                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-mgr-cod-arc       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Se errore : set del flag                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-cli-rag-soc         .
      *              *-------------------------------------------------*
      *              * Chiusura file [cli]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       let-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Lettura [fnt]                                             *
      *    *-----------------------------------------------------------*
       let-cod-fnt-000.
      *              *-------------------------------------------------*
      *              * Se codice a zero                                *
      *              *-------------------------------------------------*
           if        rf-mgr-cod-arc       =    zero
                     move  spaces         to   rf-fnt-rag-soc
                     go to let-cod-fnt-999.
      *              *-------------------------------------------------*
      *              * Apertura file [fnt]                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * Lettura file [fnt]                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      rf-mgr-cod-arc       to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * Se errore : set del flag                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-fnt-rag-soc         .
      *              *-------------------------------------------------*
      *              * Chiusura file [fnt]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       let-cod-fnt-999.
           exit.

      *    *===========================================================*
      *    * Lettura [pdc]                                             *
      *    *-----------------------------------------------------------*
       let-cod-pdc-000.
      *              *-------------------------------------------------*
      *              * Se codice a zero                                *
      *              *-------------------------------------------------*
           if        rf-mgr-cod-pdc       =    zero
                     move  spaces         to   rf-pdc-des-pdc
                     go to let-cod-pdc-999.
      *              *-------------------------------------------------*
      *              * Apertura file [pdc]                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Lettura file [pdc]                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSTC"             to   f-key                  .
           move      rf-mgr-cod-pdc       to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *              *-------------------------------------------------*
      *              * Se errore : set del flag                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-pdc-des-pdc         .
      *              *-------------------------------------------------*
      *              * Chiusura file [pdc]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
       let-cod-pdc-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

