       Identification Division.
       Program-Id.                                 mbolxml0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orc                 *
      *                                Settore:    ric                 *
      *                                   Fase:    bolxml              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 15/06/23    *
      *                       Ultima revisione:    NdK del 05/09/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Preparazione Documenti di Trasporto in      *
      *                    formato XML per EDI Metel (DESADV)          *
      *                                                                *
      *                    ___ DA COMPLETARE (05/09/23) ___            *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * N.B.: Attualmente basato su tracciati METEL per EDI (ELETTRA)  *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Percorso       :   '/abd/asc/exp/edi/'                         *
      *                                                                *
      * Nome file      :   'DESADV_AA_NNNNNN_AAMMGGHHMM.xml'           *
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
      *        Input  : m-bol-xml-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : m-bol-xml-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : m-bol-xml-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : m-bol-xml-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "GF" - Generazione file                                        *
      *                                                                *
      *                                                                *
      *        Input  : m-bol-xml-tip-ope = "GF"                       *
      *                                                                *
      *                 m-bol-xml-prt-bit = protocollo ordine cliente  *
      *                                                                *
      *                                                                *
      *        Output : m-bol-xml-exi-sts = spaces: generazione ese-   *
      *                                             guita              *
      *                                     #     : errore             *
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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

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
      *    * Area di comunicazione per modulo                 "mopsys" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential, con pathname lungo               *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/j"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [bit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .
      *        *-------------------------------------------------------*
      *        * [bir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .
      *        *-------------------------------------------------------*
      *        * [bix]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbix"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .

      *    *===========================================================*
      *    * Work generica per il programma                            *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Carattere di fine record                              *
      *        *-------------------------------------------------------*
           05  w-gen-chr-end              pic  x(01)   value H"0D"    .
      *        *-------------------------------------------------------*
      *        * Data attuale                                          *
      *        *-------------------------------------------------------*
           05  w-gen-dat-att              pic  9(07)                  .
           05  w-gen-dat-att-r redefines
               w-gen-dat-att.
               10  w-gen-sec-att          pic  9(01)                  .
               10  w-gen-aaa-att          pic  9(02)                  .
               10  w-gen-mmm-att          pic  9(02)                  .
               10  w-gen-ggg-att          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Ora attuale                                           *
      *        *-------------------------------------------------------*
           05  w-gen-tim-att              pic  9(08)                  .
           05  w-gen-tim-att-r redefines
               w-gen-tim-att.
               10  w-gen-ora-att          pic  9(02)                  .
               10  w-gen-min-att          pic  9(02)                  .
               10  w-gen-sec-att          pic  9(02)                  .
               10  w-gen-cen-att          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Timestamp                                             *
      *        *-------------------------------------------------------*
           05  w-gen-tim-stm              pic  9(10)                  .
           05  w-gen-tim-stm-r redefines
               w-gen-tim-stm.
               10  w-gen-tim-aaa          pic  9(02)                  .
               10  w-gen-tim-mmm          pic  9(02)                  .
               10  w-gen-tim-ggg          pic  9(02)                  .
               10  w-gen-tim-hhh          pic  9(02)                  .
               10  w-gen-tim-min          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Si/No programma richiamato in batch                   *
      *        *-------------------------------------------------------*
           05  w-gen-snx-btc              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per editing                                    *
      *        *-------------------------------------------------------*
           05  w-gen-ann-doc              pic  x(02)                  .
           05  w-gen-num-doc              pic  x(06)                  .
           05  w-gen-tim-stp              pic  x(10)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-naz      pic  x(03)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-cmn      pic  9(05)                  .
               10  w-let-arc-cli-cge      pic  9(07)                  .
               10  w-let-arc-cli-piv      pic  9(11)                  .
               10  w-let-arc-cli-cfi      pic  x(16)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxc.
               10  w-let-arc-gxc-flg      pic  x(01)                  .
               10  w-let-arc-gxc-tip      pic  x(01)                  .
               10  w-let-arc-gxc-cmn      pic  9(05)                  .
               10  w-let-arc-gxc-fzn      pic  9(03)                  .
               10  w-let-arc-gxc-lct      pic  9(03)                  .
               10  w-let-arc-gxc-des      pic  x(30)                  .
               10  w-let-arc-gxc-prv      pic  x(02)                  .
               10  w-let-arc-gxc-cap      pic  x(05)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det pathname file sequenziale in output      *
      *        *-------------------------------------------------------*
           05  w-det-pth-fso.
               10  w-det-pth-fso-nam      pic  x(40)                  .
               10  w-det-pth-fso-pth      pic  x(60)                  .

      *    *===========================================================*
      *    * Work-area per stampa indirizzo di spedizione              *
      *    *-----------------------------------------------------------*
       01  w-ind-spe.
           05  w-ind-spe-rag              pic  x(40)                  .
           05  w-ind-spe-rs2              pic  x(40)                  .
           05  w-ind-spe-via              pic  x(40)                  .
           05  w-ind-spe-loc              pic  x(40)                  .
           05  w-ind-spe-prv              pic  x(02)                  .
           05  w-ind-spe-cty              pic  x(30)                  .
           05  w-ind-spe-cap              pic  x(05)                  .
           05  w-ind-spe-naz              pic  x(03)                  .

      *    *===========================================================*
      *    * Work-area per numero documento                            *
      *    *-----------------------------------------------------------*
       01  w-num-doc.
      *        *-------------------------------------------------------*
      *        * Work per ridefinizione                                *
      *        *-------------------------------------------------------*
           05  w-num-doc-num              pic  9(11)                  .
           05  w-num-doc-num-r redefines
               w-num-doc-num.
               10  w-num-doc-saa          pic  9(03)                  .
               10  w-num-doc-ngi          pic  9(02)                  .
               10  w-num-doc-prg          pic  9(06)                  .

      *    *===========================================================*
      *    * Work per scrittura records in file sequenziale di appog-  *
      *    * gio                                                       *
      *    *-----------------------------------------------------------*
       01  w-scr-fso.
      *        *-------------------------------------------------------*
      *        * Carattere di fine riga                                *
      *        *-------------------------------------------------------*
           05  w-scr-fso-cfr              pic  x(01)   value H"0D"    .

      *    *===========================================================*
      *    * Work area per emissione tag in formato 'xml'              *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wtagxml0.cpw"                   .

      *    *===========================================================*
      *    * Work per definizione record conferme ordine via EDI       *
      *    *-----------------------------------------------------------*
       01  w-rec.
           05  w-rec-num-rig              pic  9(03)                  .
           05  w-rec-num-com              pic  9(03)                  .
           05  w-rec-num-add              pic  9(03)                  .
           05  w-rec-prz-edt              pic  x(10)                  .
           05  w-rec-val-edt              pic  x(12)                  .
           05  w-rec-qta-edt              pic  x(12)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

      *    *===========================================================*
      *    * Work-area per trattamento stringhe molto lunghe           *
      *    * 								                           *
      *    * N.B.: tratto da 'wallstr0'                                *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wbigstr0.cpw"                   .

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
      *    * Area di comunicazione per generazione ordini XML          *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/mbolxml0.mdl"                   .

      ******************************************************************
       Procedure Division                using m-bol-xml              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   m-bol-xml-exi-sts      .
       main-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        m-bol-xml-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   m-bol-xml-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   m-bol-xml-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Generazione file XML                        *
      *                  *---------------------------------------------*
           else if   m-bol-xml-tip-ope    =    "EF"
                     perform gfx-000      thru gfx-999                .
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
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *              *-------------------------------------------------*
      *              * [bit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * [bir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * [bix]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
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
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *              *-------------------------------------------------*
      *              * [bit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * [bir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * [bix]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *              *-------------------------------------------------*
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
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
                     move  spaces         to   m-bol-xml-exi-sts
           else      move  "#"            to   m-bol-xml-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *-----------------------------------------------------------*
       gfx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione pathname completo file XML      *
      *              *-------------------------------------------------*
           move      spaces               to   m-bol-xml-pth-xml      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record [bit]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * Lettura record [bit]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT     "        to   f-key                  .
           move      m-bol-xml-prt-bit    to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
       gfx-100.
      *              *-------------------------------------------------*
      *              * Open file sequenziale                           *
      *              *-------------------------------------------------*
           perform   gfx-seq-000          thru gfx-seq-999            .
      *              *-------------------------------------------------*
      *              * Scrittura intestazione                          *
      *              *-------------------------------------------------*
           perform   gfx-int-000          thru gfx-int-999            .
      *              *-------------------------------------------------*
      *              * Scrittura testata                               *
      *              *-------------------------------------------------*
           perform   gfx-tes-000          thru gfx-tes-999            .
      *              *-------------------------------------------------*
      *              * Scrittura indirizzo                             *
      *              *-------------------------------------------------*
           perform   gfx-adr-000          thru gfx-adr-999            .
       gfx-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori righe                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-rec-num-rig          .
           move      zero                 to   w-rec-num-com          .
           move      zero                 to   w-rec-num-add          .
      *              *-------------------------------------------------*
      *              * Start su file [bir]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      m-bol-xml-prt-bit    to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to gfx-800.
       gfx-500.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [bir]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : a spese                      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to gfx-600.
       gfx-530.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bir-num-prt       not  = m-bol-xml-prt-bit
                     go to gfx-600.
       gfx-540.
      *              *-------------------------------------------------*
      *              * Selezione sul record                            *
      *              *-------------------------------------------------*
       gfx-550.
      *              *-------------------------------------------------*
      *              * Scrittura riga fattura cliente                  *
      *              *-------------------------------------------------*
           perform   gfx-rig-000          thru gfx-rig-999            .
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale [bir]            *
      *              *-------------------------------------------------*
           go to     gfx-500.
       gfx-600.
      *              *-------------------------------------------------*
      *              * Chiusura tag <Document>                         *
      *              *-------------------------------------------------*
           move      "Document"           to   w-sta-emi-xml-pmt      .
           move      "C"                  to   w-sta-emi-xml-ope      .
           move      "-"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-800.
      *              *-------------------------------------------------*
      *              * Chiusura file di output                         *
      *              *-------------------------------------------------*
           move      "CL"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mcvoju"
                                         using j                      .
       gfx-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-999.
       gfx-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Apertura file sequenziale                                 *
      *    *-----------------------------------------------------------*
       gfx-seq-000.
      *              *-------------------------------------------------*
      *              * Preparazione data attuale                       *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-gen-dat-att          .
           move      s-tim                to   w-gen-tim-att          .
       gfx-seq-100.
      *              *-------------------------------------------------*
      *              * Determinazione pathname                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing anno documento                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-bit-num-doc
                    (02 : 02)             to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-gen-ann-doc          .
      *                  *---------------------------------------------*
      *                  * Editing numero documento minimo             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-bit-num-doc
                    (06 : 06)             to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-gen-num-doc          .
      *                  *---------------------------------------------*
      *                  * Determinazione timestamp                    *
      *                  *---------------------------------------------*
           move      w-gen-aaa-att        to   w-gen-tim-aaa          .
           move      w-gen-mmm-att        to   w-gen-tim-mmm          .
           move      w-gen-ggg-att        to   w-gen-tim-ggg          .
           move      w-gen-ora-att        to   w-gen-tim-hhh          .
           move      w-gen-min-att        to   w-gen-tim-min          .
      *                  *---------------------------------------------*
      *                  * Editing timestamp                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-gen-tim-stm        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-gen-tim-stp          .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-pth-fso-nam      .
           string    "DESADV_"  delimited by   size
                     w-gen-ann-doc
                                delimited by   spaces 
                     "_"        delimited by   size
                     w-gen-num-doc
                                delimited by   spaces
                     "_"        delimited by   size
                     w-gen-tim-stp
                                delimited by   spaces
                     ".xml"     delimited by   size
                                          into w-det-pth-fso-nam      .
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           perform   det-pth-fso-000      thru det-pth-fso-999        .
      *                  *---------------------------------------------*
      *                  * Apertura del file in output                 *
      *                  *---------------------------------------------*
           move      "OO"                 to   j-ope                  .
           move      "seq "               to   j-nam                  .
           move      w-det-pth-fso-pth    to   j-pat                  .
           call      "swd/mod/prg/obj/mcvoju"
                                         using j                      .
       gfx-seq-800.
      *              *-------------------------------------------------*
      *              * Memorizzazione pathname completo file XML       *
      *              *-------------------------------------------------*
           move      w-det-pth-fso-pth    to   m-bol-xml-pth-xml      .
       gfx-seq-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-seq-999.
       gfx-seq-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Scrittura intestazione documento                          *
      *    *-----------------------------------------------------------*
       gfx-int-000.
      *              *-------------------------------------------------*
      *              * Letture accessorie                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [ada]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Lettura [ada] Sede                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      zero                 to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
       gfx-int-100.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Indice di indentazione XML                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-sta-emi-xml-inx      .
       gfx-int-200.
      *              *-------------------------------------------------*
      *              * Apertura file - direttiva xml                   *
      *              *-------------------------------------------------*
           move      "<?xml version=""1.0"" encoding=""UTF-8"" standalon
      -              "e=""yes""?>"        to   j-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *              *-------------------------------------------------*
      *              * Apertura documento - direttiva xml              *
      *              *-------------------------------------------------*
           move      "<Document xmlns:xsi=""http://www.w3.org/2001/XMLSc
      -              "hema-instance"">"   to   j-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       gfx-int-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-int-999.
       gfx-int-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Scrittura testata ordine cliente                          *
      *    *-----------------------------------------------------------*
       gfx-tes-000.
      *              *-------------------------------------------------*
      *              * Letture accessorie                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [bix]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [bix]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bit-num-prt       to   rf-bix-num-prt         .
           move      zero                 to   rf-bix-num-prg         .
           move      01                   to   rf-bix-tip-rec         .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
      *                  *---------------------------------------------*
      *                  * Lettura Cliente contabile                   *
      *                  *---------------------------------------------*
           move      rf-bit-cod-arc       to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
       gfx-tes-200.
      *              *-------------------------------------------------*
      *              * Apertura tag <Header>                           *
      *              *-------------------------------------------------*
           move      "Header"             to   w-sta-emi-xml-pmt      .
           move      "A"                  to   w-sta-emi-xml-ope      .
           move      "+"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *              *-------------------------------------------------*
      *              * Emissione tag <VatSeller>                       *
      *              *                                                 *
      *              * Partita Iva Azienda                             *
      *              *-------------------------------------------------*
           move      "VatSeller"          to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      rf-ada-prt-iva
                    (01 : 11)             to   w-sta-emi-xml-val      .
           move      "+"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *              *-------------------------------------------------*
      *              * Emissione tag <VatBuyer>                        *
      *              *                                                 *
      *              * Partita Iva Cliente                             *
      *              *-------------------------------------------------*
           move      "VatBuyer"           to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      rf-cli-prt-iva
                    (01 : 11)             to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-tes-250.
      *              *-------------------------------------------------*
      *              * Emissione tag <DesadvNumber>                    *
      *              *                                                 *
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      "DesadvNumber"       to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      rf-bit-num-doc 
                    (06 : 06)             to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-tes-300.
      *              *-------------------------------------------------*
      *              * Emissione tag <DesadvDate>                      *
      *              *                                                 *
      *              * Data documento                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing preliminare                         *
      *                  *---------------------------------------------*
           move      rf-bit-dat-doc       to   w-sta-emi-xml-dat      .
           move      "D"                  to   w-sta-emi-xml-ope      .
           move      " "                  to   w-sta-emi-xml-edm      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "DesadvDate"         to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-tes-500.
       gfx-tes-600.
      *              *-------------------------------------------------*
      *              * Emissione tag <DeliveryPoint>                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se indirizzo di spedizione dipendenza  *
      *                  *---------------------------------------------*
           if        rf-bit-tip-ids       not  = 04
                     go to gfx-tes-630.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente               *
      *                  *---------------------------------------------*
           move      "DeliveryPoint"      to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      rf-dcc-idn-ema       to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     gfx-tes-650.
       gfx-tes-630.
      *                  *---------------------------------------------*
      *                  * Se indirizzo di spedizione manuale o sede   *
      *                  *---------------------------------------------*
           move      "DeliveryPoint"      to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      "DP"                 to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     gfx-tes-650.
       gfx-tes-650.
       gfx-tes-800.
      *              *-------------------------------------------------*
      *              * Chiusura tag <Header>                           *
      *              *-------------------------------------------------*
           move      "Header"             to   w-sta-emi-xml-pmt      .
           move      "C"                  to   w-sta-emi-xml-ope      .
           move      "-"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-tes-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-tes-999.
       gfx-tes-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Scrittura indirizzo ordine cliente                        *
      *    *-----------------------------------------------------------*
       gfx-adr-000.
      *              *-------------------------------------------------*
      *              * Determinazione indirizzo di spedizione          *
      *              *-------------------------------------------------*
           perform   gfx-adr-ids-000      thru gfx-adr-ids-999        .
       gfx-adr-100.
      *              *-------------------------------------------------*
      *              * Apertura tag <Address>                          *
      *              *-------------------------------------------------*
           move      "Address"            to   w-sta-emi-xml-pmt      .
           move      "A"                  to   w-sta-emi-xml-ope      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-adr-200.
      *              *-------------------------------------------------*
      *              * Emissione tag <AddressQualifier>                *
      *              *-------------------------------------------------*
           move      "AddressQualifier"   to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
      *
           if        rf-bit-tip-ids       =    04 and
                     rf-dcc-idn-ema       not  = spaces
                     move  rf-dcc-idn-ema to   w-sta-emi-xml-val
           else      move  "DP"           to   w-sta-emi-xml-val      .
      *
           move      "+"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *              *-------------------------------------------------*
      *              * Emissione tag <CompanyName>                     *
      *              *                                                 *
      *              * Ragione sociale cliente                         *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-ind-spe-rag        to   w-all-str-cat (1)      .
           move      w-ind-spe-rs2        to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      "CompanyName"        to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      w-all-str-alf        to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *              *-------------------------------------------------*
      *              * Emissione tag <Address>                         *
      *              *                                                 *
      *              * Indirizzo cliente                               *
      *              *-------------------------------------------------*
           move      "Address"            to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      w-ind-spe-via        to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-adr-400.
      *              *-------------------------------------------------*
      *              * Emissione tag <City>                            *
      *              *                                                 *
      *              * Localita' cliente                               *
      *              *-------------------------------------------------*
           move      "City"               to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      w-ind-spe-cty        to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *              *-------------------------------------------------*
      *              * Emissione tag <Province>                        *
      *              *                                                 *
      *              * Provincia cliente                               *
      *              *-------------------------------------------------*
           move      "Province"           to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      w-ind-spe-prv        to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *              *-------------------------------------------------*
      *              * Emissione tag <PostalCode>                      *
      *              *                                                 *
      *              * CAP cliente                                     *
      *              *-------------------------------------------------*
           move      "PostalCode"         to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      w-ind-spe-cap        to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *              *-------------------------------------------------*
      *              * Emissione tag <Nation>                          *
      *              *                                                 *
      *              * Nazione cliente                                 *
      *              *-------------------------------------------------*
           move      "Nation"             to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      w-ind-spe-naz        to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-adr-800.
      *              *-------------------------------------------------*
      *              * Chiusura tag <Address>                          *
      *              *-------------------------------------------------*
           move      "Address"            to   w-sta-emi-xml-pmt      .
           move      "C"                  to   w-sta-emi-xml-ope      .
           move      "-"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-adr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-adr-999.
       gfx-adr-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Scrittura indirizzo ordine cliente                        *
      *    *                                                           *
      *    * Subroutine di determinazione indirizzo di spedizione      *
      *    *-----------------------------------------------------------*
       gfx-adr-ids-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo indirizzo di    *
      *              * spedizione                                      *
      *              *                                                 *
      *              * - 01 : Alla sede legale                         *
      *              * - 02 : Manuale                                  *
      *              * - 03 : Alla sede                                *
      *              * - 04 : Alla dipendenza                          *
      *              *-------------------------------------------------*
           go to     gfx-adr-ids-100
                     gfx-adr-ids-200
                     gfx-adr-ids-300
                     gfx-adr-ids-400
                                depending on   rf-bit-tip-ids         .
       gfx-adr-ids-100.
      *              *-------------------------------------------------*
      *              * Alla sede legale                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione dati da intestazione         *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-rag    to   w-ind-spe-rag          .
           move      spaces               to   w-ind-spe-rs2          .
           move      w-let-arc-cli-via    to   w-ind-spe-via          .
           move      w-let-arc-gxc-des    to   w-ind-spe-loc          .
           move      w-let-arc-gxc-prv    to   w-ind-spe-prv          .
           move      w-let-arc-gxc-des    to   w-ind-spe-cty          .
           move      w-let-arc-gxc-cap    to   w-ind-spe-cap          .
           move      w-let-arc-cli-naz    to   w-ind-spe-naz          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     gfx-adr-ids-900.
       gfx-adr-ids-200.
      *              *-------------------------------------------------*
      *              * Manuale                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori di estensione al re- *
      *                  * cord di testata                             *
      *                  *---------------------------------------------*
           move      rf-bix-rag-ids       to   w-ind-spe-rag          .
           move      rf-bix-rs2-ids       to   w-ind-spe-rs2          .
           move      rf-bix-via-ids       to   w-ind-spe-via          .
           move      rf-bix-loc-ids       to   w-ind-spe-loc          .
      *                  *---------------------------------------------*
      *                  * Estrazione elementi da localita'            *
      *                  *---------------------------------------------*
           move      rf-bix-loc-ids       to   w-all-str-alf          .
           move      " "                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (3)    to   w-ind-spe-prv          .
           move      w-all-str-cat (2)    to   w-ind-spe-cty          .
           move      w-all-str-cat (1)    to   w-ind-spe-cap          .
      *
           move      "IT"                 to   w-ind-spe-naz          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     gfx-adr-ids-900.
       gfx-adr-ids-300.
      *              *-------------------------------------------------*
      *              * Alla sede                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcc]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-bit-cod-arc       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori letti                *
      *                  *---------------------------------------------*
           move      rf-dcc-rs1-doc       to   w-ind-spe-rag          .
           move      rf-dcc-rs2-doc       to   w-ind-spe-rs2          .
           move      rf-dcc-via-dcc       to   w-ind-spe-via          .
           move      rf-dcc-loc-dcc       to   w-ind-spe-loc          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori letti da [gxc]       *
      *                  *---------------------------------------------*
           move      "C"                  to   w-let-arc-gxc-tip      .
           move      rf-dcc-cod-cmn       to   w-let-arc-gxc-cmn      .
           move      zero                 to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *
           move      w-let-arc-gxc-prv    to   w-ind-spe-prv          .
           move      w-let-arc-gxc-des    to   w-ind-spe-cty          .
           move      w-let-arc-gxc-cap    to   w-ind-spe-cap          .
      *
           move      rf-dcc-cod-naz       to   w-ind-spe-naz          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     gfx-adr-ids-900.
       gfx-adr-ids-400.
      *              *-------------------------------------------------*
      *              * Alla dipendenza                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcc]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-bit-cod-arc       to   rf-dcc-cod-cli         .
           move      rf-bit-dpz-arc       to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori letti                *
      *                  *---------------------------------------------*
           move      rf-dcc-rs1-doc       to   w-ind-spe-rag          .
           move      rf-dcc-rs2-doc       to   w-ind-spe-rs2          .
           move      rf-dcc-via-dcc       to   w-ind-spe-via          .
           move      rf-dcc-loc-dcc       to   w-ind-spe-loc          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori letti da [gxc]       *
      *                  *---------------------------------------------*
           move      "C"                  to   w-let-arc-gxc-tip      .
           move      rf-dcc-cod-cmn       to   w-let-arc-gxc-cmn      .
           move      zero                 to   w-let-arc-gxc-fzn      .
           move      zero                 to   w-let-arc-gxc-lct      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *
           move      w-let-arc-gxc-prv    to   w-ind-spe-prv          .
           move      w-let-arc-gxc-des    to   w-ind-spe-cty          .
           move      w-let-arc-gxc-cap    to   w-ind-spe-cap          .
      *
           move      rf-dcc-cod-naz       to   w-ind-spe-naz          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     gfx-adr-ids-900.
       gfx-adr-ids-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-adr-ids-999.
       gfx-adr-ids-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Scrittura riga                                            *
      *    *-----------------------------------------------------------*
       gfx-rig-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di riga         *
      *              *-------------------------------------------------*
           if        rf-bir-tip-rig (1:1) =    "P"
                     perform gfx-rig-pro-000
                                          thru gfx-rig-pro-999
           else if   rf-bir-tip-rig (1:1) =    "C"
                     perform gfx-rig-com-000
                                          thru gfx-rig-com-999
           else if   rf-bir-tip-rig (1:1) =    "A"
                     perform gfx-rig-add-000
                                          thru gfx-rig-add-999
           else      go to gfx-rig-900.
       gfx-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-rig-999.
       gfx-rig-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Subroutine per le righe di Prodotto                       *
      *    *                                                           *
      *    * Sotto-elementi per la riga <LineItem>:                    *
      *    *                                                           *
      *    * <LineNumber>        = Numero di riga                      *
      *    * <ItemCode>          = Nostro codice alfanumerico prodotto *
      *    * <OrderedQuantity>   = Quantita' ordinata                  *
      *    * <MeasureUnit>       = Unita' di misura (PCE)              *
      *    * <DeliveredQuantity> = Quantita' consegnata                *
      *    * <OrderNumber>       = Riferimento numero ordine           *
      *    * <OrderDate>         = Riferimento data ordine             *
      *    * <OrderLineNumber>   = Riferimento riga ordine             *
      *    * <BuyerItemCode>     = Codice prodotto per il Cliente      *
      *    *-----------------------------------------------------------*
       gfx-rig-pro-000.
      *              *-------------------------------------------------*
      *              * Incremento numero riga                          *
      *              *-------------------------------------------------*
           add       1                    to   w-rec-num-rig          .
       gfx-rig-pro-100.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [dcp]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Lettura per codice                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      rf-bir-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Normalizzazione [pdx]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                  *---------------------------------------------*
      *                  * Lettura [pdx]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           move      05                   to   rf-pdx-tip-rec         .
           move      rf-bir-cod-arc       to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      rf-bir-num-pro       to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      01                   to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
       gfx-rig-pro-200.
      *              *-------------------------------------------------*
      *              * Apertura tag <LineItem>                         *
      *              *-------------------------------------------------*
           move      "LineItem"           to   w-sta-emi-xml-pmt      .
           move      "A"                  to   w-sta-emi-xml-ope      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-225.
      *              *-------------------------------------------------*
      *              * Emissione tag <LineNumber>                      *
      *              *                                                 *
      *              * Progressivo riga                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing preliminare                         *
      *                  *---------------------------------------------*
           move      w-rec-num-rig        to   w-sta-emi-xml-num      .
           move      "N"                  to   w-sta-emi-xml-ope      .
           move      03                   to   w-sta-emi-xml-car      .
           move      zero                 to   w-sta-emi-xml-dec      .
           move      spaces               to   w-sta-emi-xml-sgn      .
           move      "<B"                 to   w-sta-emi-xml-edm      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "LineNumber"         to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      "+"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-300.
      *              *-------------------------------------------------*
      *              * Emissione tag <ItemCode>                        *
      *              *                                                 *
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           move      "ItemCode"           to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      rf-bir-alf-pro       to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-350.
      *              *-------------------------------------------------*
      *              * Emissione tag <OrderedQuantity>                 *
      *              *                                                 *
      *              * Quantita' ordinata                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [ocr]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [ocr]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT     "        to   f-key                  .
           move      rf-bir-coc-prt       to   rf-ocr-num-prt         .
           move      rf-bir-coc-prg       to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Editing preliminare                         *
      *                  *---------------------------------------------*
           move      "N"                  to   w-sta-emi-xml-ope      .
           move      rf-ocr-qta-ord       to   w-sta-emi-xml-num      .
           move      09                   to   w-sta-emi-xml-car      .
           move      03                   to   w-sta-emi-xml-dec      .
           move      spaces               to   w-sta-emi-xml-sgn      .
           move      "<B"                 to   w-sta-emi-xml-edm      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "OrderedQuantity"    to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-400.
      *              *-------------------------------------------------*
      *              * Emissione tag <MeasureUnit>                     *
      *              *                                                 *
      *              * Unita' di misura                                *
      *              *-------------------------------------------------*
           move      "MeasureUnit"        to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      "PCE"                to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-500.
      *              *-------------------------------------------------*
      *              * Emissione tag <DeliveredQuantity>               *
      *              *                                                 *
      *              * Quantita' consegnata                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing preliminare                         *
      *                  *---------------------------------------------*
           move      "N"                  to   w-sta-emi-xml-ope      .
           move      rf-bir-qta-ven       to   w-sta-emi-xml-num      .
           move      09                   to   w-sta-emi-xml-car      .
           move      03                   to   w-sta-emi-xml-dec      .
           move      spaces               to   w-sta-emi-xml-sgn      .
           move      "<B"                 to   w-sta-emi-xml-edm      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "DeliveredQuantity"  to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-600.
      *              *-------------------------------------------------*
      *              * Emissione tag <OrderNumber>                     *
      *              *                                                 *
      *              * Riferimento numero ordine                       *
      *              *-------------------------------------------------*
           move      "OrderNumber"        to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      rf-bir-coc-num
                    (06 : 06)             to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-650.
      *              *-------------------------------------------------*
      *              * Emissione tag <OrderDate>                       *
      *              *                                                 *
      *              * Riferimento data ordine                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing preliminare                         *
      *                  *---------------------------------------------*
           move      rf-bir-coc-dat       to   w-sta-emi-xml-dat      .
           move      "D"                  to   w-sta-emi-xml-ope      .
           move      " "                  to   w-sta-emi-xml-edm      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "OrderDate"          to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-700.
      *              *-------------------------------------------------*
      *              * Emissione tag <OrderLineNumber>                 *
      *              *                                                 *
      *              * Riferimento riga ordine                         *
      *              *                                                 *
      *              * ___ rf-bir-coc-prg ___ ??? ___                  *
      *              *-------------------------------------------------*



       gfx-rig-pro-800.
      *              *-------------------------------------------------*
      *              * Emissione tag <BuyerItemCode>                   *
      *              *                                                 *
      *              * Codice prodotto per il Cliente                  *
      *              *                                                 *
      *              * N.B.: leggere eventualmente [pdx] per codici    *
      *              *       oltre i 14 caratteri                      *
      *              *-------------------------------------------------*
           move      "BuyerItemCode"      to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
      *
           if        rf-bir-cop-scl       not  = spaces
                     move  rf-bir-cop-scl to   w-sta-emi-xml-val
           else      move  rf-pdx-des-pro to   w-sta-emi-xml-val      .
      *
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-850.
      *              *-------------------------------------------------*
      *              * Chiusura tag <LineItem>                         *
      *              *-------------------------------------------------*
           move      "LineItem"           to   w-sta-emi-xml-pmt      .
           move      "C"                  to   w-sta-emi-xml-ope      .
           move      "-"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-rig-pro-999.
       gfx-rig-pro-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Subroutine per le righe di Commento                       *
      *    *                                                           *
      *    * Sotto-elementi per la riga prodotto:                      *
      *    *                                                           *
      *    * <LineFreeText>      = Inizio testo libero                 *
      *    * <LineFreeNote>      = Testo libero                        *
      *    *                                                           *
      *    * N.B.: i commenti sono "legati" alla riga prodotto ___ !!! *
      *    *-----------------------------------------------------------*
       gfx-rig-com-000.
      *              *-------------------------------------------------*
      *              * Test se si tratta della prima riga in assoluto  *
      *              * dell'ordine                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-rec-num-com        >    zero or
                     w-rec-num-rig        >    zero or
                     w-rec-num-add        >    zero
                     go to gfx-rig-com-050.
      *                  *---------------------------------------------*
      *                  * Emissione commento legato alla testata      *
      *                  *---------------------------------------------*
           perform   gfx-rig-clt-000      thru gfx-rig-clt-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     gfx-rig-com-900.
       gfx-rig-com-050.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe di commento          *
      *              *-------------------------------------------------*
           add       1                    to   w-rec-num-com          .
       gfx-rig-com-100.
      *              *-------------------------------------------------*
      *              * Apertura tag <LineFreeText>                     *
      *              *-------------------------------------------------*
           move      "LineFreeText"       to   w-sta-emi-xml-pmt      .
           move      "A"                  to   w-sta-emi-xml-ope      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-com-200.
      *              *-------------------------------------------------*
      *              * Emissione tag <LineFreeNote>                    *
      *              *                                                 *
      *              * Linea descrittiva                               *
      *              *-------------------------------------------------*
           move      "LineFreeNote"       to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      rf-bir-des-rig       to   w-sta-emi-xml-val      .
           move      "+"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-com-800.
      *              *-------------------------------------------------*
      *              * Chiusura tag <LineFreeText>                     *
      *              *-------------------------------------------------*
           move      "LineFreeText"       to   w-sta-emi-xml-pmt      .
           move      "C"                  to   w-sta-emi-xml-ope      .
           move      "-"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-com-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-rig-com-999.
       gfx-rig-com-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Subroutine per la prima riga di Commento accessoria alla  *
      *    * testata                                                   *
      *    *                                                           *
      *    * Sotto-elementi per la testata:                            *
      *    *                                                           *
      *    * <HeaderFreeText>    = Inizio testo libero                 *
      *    *   <TextQualifier>   = "1"                                 *
      *    *   <HeaderFreeNote>  = Testo libero                        *
      *    *-----------------------------------------------------------*
       gfx-rig-clt-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe di commento          *
      *              *-------------------------------------------------*
           add       1                    to   w-rec-num-com          .
       gfx-rig-clt-100.
      *              *-------------------------------------------------*
      *              * Apertura tag <HeaderFreeText>                   *
      *              *-------------------------------------------------*
           move      "HeaderFreeText"     to   w-sta-emi-xml-pmt      .
           move      "A"                  to   w-sta-emi-xml-ope      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-clt-200.
      *              *-------------------------------------------------*
      *              * Emissione tag <TextQualifier>                   *
      *              *                                                 *
      *              * Attualmente gestito solo "1"                    *
      *              *-------------------------------------------------*
           move      "TextQualifier"      to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      "1"                  to   w-sta-emi-xml-val      .
           move      "+"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-clt-400.
      *              *-------------------------------------------------*
      *              * Emissione tag <HeaderFreeNote>                  *
      *              *                                                 *
      *              * Linea descrittiva                               *
      *              *-------------------------------------------------*
           move      "HeaderFreeNote"     to   w-sta-emi-xml-pmt      .
           move      "E"                  to   w-sta-emi-xml-ope      .
           move      rf-bir-des-rig       to   w-sta-emi-xml-val      .
           move      "="                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-clt-800.
      *              *-------------------------------------------------*
      *              * Chiusura tag <HeaderFreeText>                   *
      *              *-------------------------------------------------*
           move      "HeaderFreeText"     to   w-sta-emi-xml-pmt      .
           move      "C"                  to   w-sta-emi-xml-ope      .
           move      "-"                  to   w-sta-emi-xml-ind      .
           perform   emi-tag-xml-000      thru emi-tag-xml-999        .
       gfx-rig-clt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-rig-clt-999.
       gfx-rig-clt-999.
           exit.

      *    *===========================================================*
      *    * Generazione file 'xml'                                    *
      *    *                                                           *
      *    * Subroutine per le righe di Addebito                       *
      *    *                                                           *
      *    * ___ ??? ___                                               *
      *    *-----------------------------------------------------------*
       gfx-rig-add-000.
      *              *-------------------------------------------------*
      *              * Incremento numero riga                          *
      *              *-------------------------------------------------*
           add       1                    to   w-rec-num-add          .
       gfx-rig-add-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gfx-rig-add-999.
       gfx-rig-add-999.
           exit.

      *    *===========================================================*
      *    * Determinazione pathname completo per il file di appoggio  *
      *    * in output                                                 *
      *    *-----------------------------------------------------------*
       det-pth-fso-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname per file completato       *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-pth-fso-pth      .
           string    "/abd/asc/exp/edi/"
                                delimited by   spaces
                     w-det-pth-fso-nam
                                delimited by   spaces
                                          into w-det-pth-fso-pth      .
       det-pth-fso-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cli-400.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
           move      rf-cli-cod-naz       to   w-let-arc-cli-naz      .
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
           move      rf-cli-cod-cmn       to   w-let-arc-cli-cmn      .
           move      rf-cli-cod-cge       to   w-let-arc-cli-cge      .
           move      rf-cli-prt-iva       to   w-let-arc-cli-piv      .
           move      rf-cli-cod-fis       to   w-let-arc-cli-cfi      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cli-flg      .
           move      all   "."            to   w-let-arc-cli-rag      .
           go to     let-arc-cli-600.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-600.
           move      spaces               to   w-let-arc-cli-naz      .
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-cmn      .
           move      zero                 to   w-let-arc-cli-cge      .
           move      zero                 to   w-let-arc-cli-piv      .
           move      spaces               to   w-let-arc-cli-cfi      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxc]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione secondo il tipo elemento             *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-tip    =    "C"
                     go to let-arc-gxc-100
           else if   w-let-arc-gxc-tip    =    "F"
                     go to let-arc-gxc-200
           else if   w-let-arc-gxc-tip    =    "L"
                     go to let-arc-gxc-300.
       let-arc-gxc-100.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Comune                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Comune                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Frazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-fzn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Frazione                            *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-300.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Localita'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-lct    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Localita'                           *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      w-let-arc-gxc-lct    to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-400.
      *              *-------------------------------------------------*
      *              * Se codice elemento a zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
           move      spaces               to   w-let-arc-gxc-cap      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Se codice elemento non esistente                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
           move      spaces               to   w-let-arc-gxc-cap      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a non trovato                *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-arc-gxc-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-600.
      *              *-------------------------------------------------*
      *              * Se codice elemento esistente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione in work area                *
      *                  *---------------------------------------------*
           move      rf-gxc-des-cfl       to   w-let-arc-gxc-des      .
           move      rf-gxc-cod-prv       to   w-let-arc-gxc-prv      .
           move      rf-gxc-cap-avp       to   w-let-arc-gxc-cap      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-999.
           exit.

      *    *===========================================================*
      *    * Put next generica per output in sequenziale               *
      *    *-----------------------------------------------------------*
       put-nxt-out-000.
      *              *-------------------------------------------------*
      *              * Scrittura su sequenziale                        *
      *              *-------------------------------------------------*
           move      "PN"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mcvoju"
                                         using j                      .
       put-nxt-out-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per emissione tag in formato 'xml'            *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wtagxml0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per trattamento stringhe molto lunghe         *
      *    *                                                           *
      *    * N.B.: tratto da 'wallstr0'                                *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wbigstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


