      *    *===========================================================*
      *    * Area [orc] Ultimo aggiornamento: 17/09/25                 *
      *    *===========================================================*
      *    *===========================================================*
      *    * Esportazione [ocf]                                        *
      *    *-----------------------------------------------------------*
       exe-exp-ocf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "ocf "               to   f-xxx-nam              .
           move      "std_orc_ocf"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-ocf-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-ocf-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-ocf-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-ocf-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [ocf]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocf                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-ocf-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-ocf-999.
       exe-exp-ocf-100.
      *              *-------------------------------------------------*
      *              * Start su file [ocf]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      zero                 to   rf-ocf-num-prt         .
           move      zero                 to   rf-ocf-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocf                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocf-800.
       exe-exp-ocf-200.
      *              *-------------------------------------------------*
      *              * Next su [ocf]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocf                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocf-800.
       exe-exp-ocf-300.
      *              *-------------------------------------------------*
      *              * Max su [ocf]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocf-400.
      *              *-------------------------------------------------*
      *              * Sel su [ocf]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocf-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-ocf-800.
       exe-exp-ocf-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-ocf-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * Protocollo                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocf-num-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo                                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocf-num-prg       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Protocollo forecast                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocf-frc-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo forecast                        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocf-frc-prg       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Quantita' evasa                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocf-qta-eva       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocf-tmo-orc       to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Dipendenza                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocf-cod-dpz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-ocf-dat-doc       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocf-num-doc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Nf"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-ocf-690.
      *              *-------------------------------------------------*
      *              * Composizione carattere di fine record           *
      *              *-------------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-ocf-700.
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-exp-ocf-720.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-exp-ocf-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [ocf]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-ocf-200.
       exe-exp-ocf-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [ocf]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocf                 .
       exe-exp-ocf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-ocf-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-ocf-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [ocp]                                        *
      *    *-----------------------------------------------------------*
       exe-exp-ocp-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "ocp "               to   f-xxx-nam              .
           move      "std_orc_ocp"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-ocp-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-ocp-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-ocp-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-ocp-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [ocp]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-ocp-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-ocp-999.
       exe-exp-ocp-100.
      *              *-------------------------------------------------*
      *              * Start su file [ocp]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      zero                 to   rf-ocp-num-prt         .
           move      zero                 to   rf-ocp-num-prg         .
           move      zero                 to   rf-ocp-prg-frm         .
           move      "pgm/orc/fls/ioc/obj/iofocp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocp-800.
       exe-exp-ocp-200.
      *              *-------------------------------------------------*
      *              * Next su [ocp]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocp-800.
       exe-exp-ocp-300.
      *              *-------------------------------------------------*
      *              * Max su [ocp]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocp-400.
      *              *-------------------------------------------------*
      *              * Sel su [ocp]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocp-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-ocp-800.
       exe-exp-ocp-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-ocp-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * Protocollo                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocp-num-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo                                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocp-num-prg       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo frammento                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocp-prg-frm       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Quantita' assegnata                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocp-qta-ass       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data assegnazione                           *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-ocp-dat-ass       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Protocollo di riferimento                   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocp-prt-orf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo di riferimento                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocp-prg-orf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Flag di pulizia                             *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocp-flg-pul       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Af"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-ocp-690.
      *              *-------------------------------------------------*
      *              * Composizione carattere di fine record           *
      *              *-------------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-ocp-700.
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-exp-ocp-720.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-exp-ocp-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [ocp]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-ocp-200.
       exe-exp-ocp-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [ocp]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
       exe-exp-ocp-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-ocp-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-ocp-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [ocr]                                        *
      *    *-----------------------------------------------------------*
       exe-exp-ocr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "ocr "               to   f-xxx-nam              .
           move      "std_orc_ocr"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-ocr-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-ocr-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-ocr-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-ocr-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-ocr-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-ocr-999.
       exe-exp-ocr-100.
      *              *-------------------------------------------------*
      *              * Start su file [ocr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      zero                 to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocr-800.
       exe-exp-ocr-200.
      *              *-------------------------------------------------*
      *              * Next su [ocr]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocr-800.
       exe-exp-ocr-300.
      *              *-------------------------------------------------*
      *              * Max su [ocr]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocr-400.
      *              *-------------------------------------------------*
      *              * Sel su [ocr]                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su data esportazione              *
      *                  *---------------------------------------------*
           if        rr-dat-exp           =    zero
                     go to exe-exp-ocr-500.
           if        rf-ocr-dat-doc       not  > rr-dat-exp
                     go to exe-exp-ocr-200.
       exe-exp-ocr-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-ocr-800.
       exe-exp-ocr-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-ocr-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * Protocollo                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-num-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo                                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-num-prg       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-tmo-orc       to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Dipendenza                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-cod-dpz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-ocr-dat-doc       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-num-doc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo ordine                                 *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-tip-ord       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           move      "E "                 to   w-scr-str-tip          .
           move      rf-ocr-tip-arc       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-dpz-arc       to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice lingua per il documento              *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-cod-lng       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data ordine cliente                         *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-ocr-ocl-dat       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-ocl-num       to   w-scr-str-ele          .
           move      10                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Priorita' di evasione                       *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-pri-eva       to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      rf-ocr-sgl-vpf       to   w-exe-sgl-vlt          .
           move      rf-ocr-dec-vpf       to   w-exe-dec-vlt          .
           move      rf-ocr-tdc-vpf       to   w-exe-tdc-vlt          .
           move      rf-ocr-cdc-vpf       to   w-exe-cdc-vlt          .
           perform   exe-gen-vlt-000      thru exe-gen-vlt-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-tip-rig       to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo magazzino                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-tip-mag       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice magazzino                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-num-pro       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico magazzino               *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-alf-pro       to   w-scr-str-ele          .
           move      14                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Variante                                    *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-sgl-vrn       to   w-scr-str-ele          .
           move      14                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice per il cliente                       *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-cop-scl       to   w-scr-str-ele          .
           move      14                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione estesa                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-des-ext       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione in riga                         *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-des-rig       to   w-scr-str-ele          .
           move      40                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo prodotto                               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-tip-pro       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Iva e contropartita generali                *
      *                  *---------------------------------------------*
           move      rf-ocr-cod-iva       to   w-exe-iec-iva          .
           move      rf-ocr-ctp-ven       to   w-exe-iec-cpt          .
           perform   exe-gen-iec-000      thru exe-gen-iec-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Unita'                                      *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-umi-ven       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Decimali quantita'                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-dec-qta       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-qta-ord       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Segnale di riga comunque saldata            *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-sdr-ccs       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Dislocazione                                *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-cod-dsl       to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/no 2. quantita'                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-snx-2qt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Decimali 2. quantita'                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-dec-2qt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * 2. quantita'                                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-qta-a02       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/no 3. quantita'                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-snx-3qt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Decimali 3. quantita'                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-dec-3qt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * 3. quantita'                                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-qta-a03       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Decimali prezzo                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-dec-prz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Valuta per prezzo standard                  *
      *                  *---------------------------------------------*
           move      rf-ocr-sgl-vps       to   w-exe-sgl-vlt          .
           move      rf-ocr-dec-vps       to   w-exe-dec-vlt          .
           move      rf-ocr-tdc-vps       to   w-exe-tdc-vlt          .
           move      rf-ocr-cdc-vps       to   w-exe-cdc-vlt          .
           perform   exe-gen-vlt-000      thru exe-gen-vlt-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Prezzo lordo standard                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-prz-lrs       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Prezzo netto standard                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-prz-nts       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Valuta per prezzo                           *
      *                  *---------------------------------------------*
           move      rf-ocr-sgl-vpp       to   w-exe-sgl-vlt          .
           move      rf-ocr-dec-vpp       to   w-exe-dec-vlt          .
           move      rf-ocr-tdc-vpp       to   w-exe-tdc-vlt          .
           move      rf-ocr-cdc-vpp       to   w-exe-cdc-vlt          .
           perform   exe-gen-vlt-000      thru exe-gen-vlt-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Flag prezzo per quantita'                   *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-flg-puq       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Prezzo                                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-prz-ven       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/no 2. prezzo                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-snx-2pz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Decimali 2. prezzo                          *
      *                  *                                             *
      *                  * Agiunta per SQL                             *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * 2. Prezzo                                   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-prz-a02       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Valuta per legame valutario                 *
      *                  *---------------------------------------------*
           move      rf-ocr-sgl-vpl       to   w-exe-sgl-lvl          .
           move      rf-ocr-dec-vpl       to   w-exe-dec-lvl          .
           move      rf-ocr-tdc-vpl       to   w-exe-tdc-lvl          .
           move      rf-ocr-prz-vpl       to   w-exe-prz-lvl          .
           move      rf-ocr-cdc-vpl       to   w-exe-cdc-lvl          .
           move      rf-ocr-ccr-vpl       to   w-exe-ccr-lvl          .
           move      rf-ocr-plm-vpl       to   w-exe-plm-lvl          .
           move      rf-ocr-tlm-vpl       to   w-exe-tlm-lvl          .
           move      rf-ocr-map-vpl       to   w-exe-map-lvl          .
           perform   exe-gen-lgv-000      thru exe-gen-lgv-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di esposizione della riga              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-epz-rgf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Categoria di sconto associata al prodotto   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-csr-aap       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-ocr-620.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    5
                     go to exe-exp-ocr-628.
      *                  *---------------------------------------------*
      *                  * % sconto                                    *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-psr-aap
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocr-620.
       exe-exp-ocr-628.
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-ocr-630.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    5
                     go to exe-exp-ocr-638.
      *                  *---------------------------------------------*
      *                  * % sconto                                    *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-per-scr
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocr-630.
       exe-exp-ocr-638.
      *                  *---------------------------------------------*
      *                  * Prezzo netto                                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-prz-net       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di esposizione prezzi e sconti         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-epz-pes       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Valuta per il costo                         *
      *                  *---------------------------------------------*
           move      rf-ocr-sgl-vpc       to   w-exe-sgl-vlt          .
           move      rf-ocr-dec-vpc       to   w-exe-dec-vlt          .
           move      rf-ocr-tdc-vpc       to   w-exe-tdc-vlt          .
           move      rf-ocr-cdc-vpc       to   w-exe-cdc-vlt          .
           perform   exe-gen-vlt-000      thru exe-gen-vlt-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Decimali costo                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-dec-cos       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Costo                                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-cos-rif       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Importo                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-imp-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Importo ausiliario                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-iau-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Categoria di provvigione al prodotto        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-cpv-aap       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-ocr-640.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    3
                     go to exe-exp-ocr-648.
      *                  *---------------------------------------------*
      *                  * % provvigione                               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-ppv-aap
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocr-640.
       exe-exp-ocr-648.
      *                  *---------------------------------------------*
      *                  * Flag di significativita' dati provvigionali *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-fsp-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Categoria di provvigione al prodotto        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-cpv-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-ocr-650.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    3
                     go to exe-exp-ocr-658.
      *                  *---------------------------------------------*
      *                  * % provvigione                               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-ppv-rig
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocr-650.
       exe-exp-ocr-658.
      *                  *---------------------------------------------*
      *                  * Importo provvigione a forfait               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-pvf-rig       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data di consegna richiesta                  *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-ocr-dcn-ric       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data di consegna prevista                   *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-ocr-dcn-prv       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data di consegna confermata                 *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-ocr-dcn-cnf       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Flag di conferma                            *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
      *
           if        rf-ocr-flg-cnf       not  = spaces
                     move "1"             to   w-scr-str-ele
           else      move "0"             to   w-scr-str-ele          .
      *
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo documento ordine                       *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocr-cmc-tip       to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data di riferimento                         *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-ocr-cmc-dat       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Numero di riferimento                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocr-cmc-num       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Segnale di riga chiusa                      *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
      *
           if        rf-ocr-flg-rch       not  = spaces
                     move "1"             to   w-scr-str-ele
           else      move "0"             to   w-scr-str-ele          .
      *
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-ocr-660.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    05
                     go to exe-exp-ocr-668.
      *                  *---------------------------------------------*
      *                  * Flag bloccanti                              *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      "0"                  to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocr-660.
       exe-exp-ocr-668.
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-ocr-670.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    05
                     go to exe-exp-ocr-678.
      *                  *---------------------------------------------*
      *                  * Flag non bloccanti                          *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      "0"                  to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocr-670.
       exe-exp-ocr-678.
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Flag di riga verificata                     *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
      *
           if        rf-ocr-flg-nbx (1)   not  = spaces
                     move "1"             to   w-scr-str-ele
           else      move "0"             to   w-scr-str-ele          .
      *
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Flag riga ordine con data prevista forzata  *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
      *
           if        rf-ocr-flg-nbx (2)   not  = spaces
                     move "1"             to   w-scr-str-ele
           else      move "0"             to   w-scr-str-ele          .
      *
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Flag di controllo in fase di spedizione     *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
      *
           if        rf-ocr-flg-nbx (3)   not  = spaces
                     move "1"             to   w-scr-str-ele
           else      move "0"             to   w-scr-str-ele          .
      *
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Nf"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-ocr-690.
      *              *-------------------------------------------------*
      *              * Composizione carattere di fine record           *
      *              *-------------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-ocr-700.
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-exp-ocr-720.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-exp-ocr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [ocr]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-ocr-200.
       exe-exp-ocr-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       exe-exp-ocr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-ocr-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-ocr-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [oct]                                        *
      *    *-----------------------------------------------------------*
       exe-exp-oct-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "oct "               to   f-xxx-nam              .
           move      "std_orc_oct"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-oct-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-oct-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-oct-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-oct-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * [zcv]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzcv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcv                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-oct-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-oct-999.
       exe-exp-oct-100.
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      rr-dat-exp           to   rf-oct-dat-doc         .
           move      zero                 to   rf-oct-cod-dpz         .
           move      zero                 to   rf-oct-num-doc         .
           move      spaces               to   rf-oct-tmo-orc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-oct-800.
       exe-exp-oct-200.
      *              *-------------------------------------------------*
      *              * Next su [oct]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-oct-800.
       exe-exp-oct-300.
      *              *-------------------------------------------------*
      *              * Max su [oct]                                    *
      *              *-------------------------------------------------*
       exe-exp-oct-400.
      *              *-------------------------------------------------*
      *              * Sel su [oct]                                    *
      *              *-------------------------------------------------*
       exe-exp-oct-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-oct-800.
       exe-exp-oct-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-oct-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * Protocollo                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-num-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-tmo-orc       to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Dipendenza                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cod-dpz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-oct-dat-doc       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-num-doc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Secolo ed anno                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-scl-ann       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Sigla numerazione                           *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-sgl-num       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo ordine (spostato qui per SQL)          *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-tip-ord       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           move      "E "                 to   w-scr-str-tip          .
           move      rf-oct-tip-arc       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-dpz-arc       to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo fornitura                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-tip-frn       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice archivio per fatturazione            *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-arc-plf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-dpz-plf       to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo fatturazione                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-tip-ftz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo indirizzo                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-tip-ids       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data di riferimento                         *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-oct-ocl-dat       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Numero di riferimento                       *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-ocl-num       to   w-scr-str-ele          .
           move      10                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riferimento                                 *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-ocl-rif       to   w-scr-str-ele          .
           move      40                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Responsabile                                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cod-rsp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data consegna prevista                      *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-oct-dat-cns       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Formato di stampa data consegna             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-fds-dtc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di evasione richiesta                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-tip-eva       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Priorita' di evasione                       *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-pri-eva       to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice condizioni di vendita                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cod-cdv       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione condizioni di vendita           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se codice presente                 *
      *                      *-----------------------------------------*
           if        rf-oct-cod-cdv       =    zero
                     move  spaces         to   rf-zcv-des-cdv
                     go to exe-exp-oct-610.
      *                      *-----------------------------------------*
      *                      * Lettura per codice                      *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCDV"             to   f-key                  .
           move      rf-oct-cod-cdv       to   rf-zcv-cod-cdv         .
           move      "pgm/orc/fls/ioc/obj/iofzcv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcv                 .
           if        f-sts                not  = e-not-err
                     move  spaces         to   rf-zcv-des-cdv         .
       exe-exp-oct-610.
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zcv-des-cdv       to   w-scr-str-ele          .
           move      120                  to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Commento ad uso interno                     *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-com-int       to   w-scr-str-ele          .
           move      120                  to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice lingua per il documento              *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-cod-lng       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      rf-oct-sgl-vpf       to   w-exe-sgl-vlt          .
           move      rf-oct-dec-vpf       to   w-exe-dec-vlt          .
           move      rf-oct-tdc-vpf       to   w-exe-tdc-vlt          .
           move      rf-oct-cdc-vpf       to   w-exe-cdc-vlt          .
           perform   exe-gen-vlt-000      thru exe-gen-vlt-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Iva e contropartita generali                *
      *                  *---------------------------------------------*
           move      rf-oct-ass-iva       to   w-exe-iec-iva          .
           move      rf-oct-ctp-ven       to   w-exe-iec-cpt          .
           perform   exe-gen-iec-000      thru exe-gen-iec-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Fattura separata                            *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-fat-sep       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Inoltro documenti                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-inl-dcm       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Inoltro pagamenti                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-inl-pgt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-cod-lst       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Categoria sconto in riga                    *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-csr-aac       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-oct-620.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    5
                     go to exe-exp-oct-628.
      *                  *---------------------------------------------*
      *                  * % sconto                                    *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-psr-aac
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-oct-620.
       exe-exp-oct-628.
      *                  *---------------------------------------------*
      *                  * Categoria sconto in chiusura                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-csc-aac       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * % sconto chiusura                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-psc-aac       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Categoria provvigioni                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cpv-aac       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-oct-630.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    3
                     go to exe-exp-oct-638.
      *                  *---------------------------------------------*
      *                  * % provvigioni                               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-ppv-aac
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-oct-630.
       exe-exp-oct-638.
      *                  *---------------------------------------------*
      *                  * VOCI DESCRITTIVE A PARTE                    *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice forma di pagamento                   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cod-fop       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * % sconto pagamento                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-scp-aap       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice ABI                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cod-abi       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice CAB                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cod-cab       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice conto corrente                       *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-ccc-app       to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice nostra banca                         *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-nos-ban       to   w-scr-str-ele          .
           move      10                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice conto corrente postale               *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-nos-ccp       to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice addebito spese incasso               *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-add-spi       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice addebito spese bollo                 *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-add-spb       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Incasso preferenziale                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-ipr-iel       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data scadenza                               *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-oct-pag-dsm       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Forfait                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-pag-qaf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Acconto                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-pag-act       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cod-age       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Flag di significativita' dati provvigionali *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-fsp-doc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Importo provvigione a forfait               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-pvf-age       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * % provvigione a forfait - SQL               *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo vendita                                *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-tip-vpa       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Categoria di provvigione agente             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cpv-aaa       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-oct-640.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    3
                     go to exe-exp-oct-648.
      *                  *---------------------------------------------*
      *                  * % provvigione                               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-ppv-aaa
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-oct-640.
       exe-exp-oct-648.
      *                  *---------------------------------------------*
      *                  * Codice intermediario                        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-cod-ime       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Importo provvigione a forfait intermediario *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-pvf-ime       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-oct-650.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    9
                     go to exe-exp-oct-658.
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-tot-rig
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-oct-650.
       exe-exp-oct-658.
      *                  *---------------------------------------------*
      *                  * Sconto chiusura                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-tot-scc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * % sconto chiusura                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-per-scc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Sconto pagamento                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-tot-scp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * % sconto pagamento                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-per-scp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * SPESE A PARTE                               *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totale documento, in valuta                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-tot-doc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Numero di stampe eseguite                   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-ctr-stp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Flag di ordine verificato                   *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
      *
           if        rf-oct-flg-nbx (1)   not  = spaces
                     move "1"             to   w-scr-str-ele
           else      move "0"             to   w-scr-str-ele          .
      *
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Flag di ordine completamente chiuso         *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
      *
           if        rf-oct-flg-och       not  = spaces
                     move "1"             to   w-scr-str-ele
           else      move "0"             to   w-scr-str-ele          .
      *
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-oct-660.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    05
                     go to exe-exp-oct-668.
      *                  *---------------------------------------------*
      *                  * Flag bloccanti                              *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      "0"                  to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-oct-660.
       exe-exp-oct-668.
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-oct-670.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    05
                     go to exe-exp-oct-678.
      *                  *---------------------------------------------*
      *                  * Flag non bloccanti                          *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      "0"                  to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-oct-670.
       exe-exp-oct-678.
      *                  *---------------------------------------------*
      *                  * Flag di pulizia                             *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      "0"                  to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *=============================================*
      *                  * Data ultima modifica                        *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-oct-ide-dat       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Utente ultima modifica                      *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-ide-ute       to   w-scr-str-ele          .
           move      08                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Fase ultima modifica                        *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-ide-fas       to   w-scr-str-ele          .
           move      06                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Af"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-oct-690.
      *              *-------------------------------------------------*
      *              * Composizione carattere di fine record           *
      *              *-------------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-oct-700.
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-exp-oct-720.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-exp-oct-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [oct]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-oct-200.
       exe-exp-oct-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * [zcv]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzcv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcv                 .
       exe-exp-oct-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-oct-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-oct-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [oct] - spese                                *
      *    *-----------------------------------------------------------*
       exe-exp-ocs-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "ocs "               to   f-xxx-nam              .
           move      "std_orc_ocs"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-ocs-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-ocs-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-ocs-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-ocs-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-ocs-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-ocs-999.
       exe-exp-ocs-100.
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      rr-dat-exp           to   rf-oct-dat-doc         .
           move      zero                 to   rf-oct-cod-dpz         .
           move      zero                 to   rf-oct-num-doc         .
           move      spaces               to   rf-oct-tmo-orc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocs-800.
       exe-exp-ocs-200.
      *              *-------------------------------------------------*
      *              * Next su [oct]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocs-800.
       exe-exp-ocs-300.
      *              *-------------------------------------------------*
      *              * Max su [oct]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocs-400.
      *              *-------------------------------------------------*
      *              * Sel su [oct]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocs-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-ocs-800.
       exe-exp-ocs-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-ocs-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-ocs-620.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    6
                     go to exe-exp-ocs-750.
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        rf-oct-spe-snx
                    (w-exe-ctr-001)       =    zero and
                     rf-oct-spe-per
                    (w-exe-ctr-001)       =    zero and
                     rf-oct-spe-imp
                    (w-exe-ctr-001)       =    zero
                     go to exe-exp-ocs-620.
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * Protocollo                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-num-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo                                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-ctr-001        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/no spesa                                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-spe-snx
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Modalita' di addeocto                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-spe-mad
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * % spesa                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-spe-per
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo imponibile                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-spe-ibl
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-002          .
       exe-exp-ocs-630.
           add       1                    to   w-exe-ctr-002          .
           if        w-exe-ctr-002        >    9
                     go to exe-exp-ocs-638.
      *                  *---------------------------------------------*
      *                  * Flag totali                                 *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-ibx-spe
                    (w-exe-ctr-001,
                     w-exe-ctr-002)       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           if        w-exe-ctr-002        =    9
                     move  "AN"           to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocs-630.
       exe-exp-ocs-638.
      *                  *---------------------------------------------*
      *                  * Importo spesa                               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-spe-imp
                    (w-exe-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice IVA spesa - SQL                      *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice contropartita spesa - SQL            *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Nf"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Composizione carattere di fine record       *
      *                  *---------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti           *
      *                  *---------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocs-620.
       exe-exp-ocs-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [oct]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-ocs-200.
       exe-exp-ocs-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
       exe-exp-ocs-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-ocs-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-ocs-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [oct] - voci descrittive                     *
      *    *-----------------------------------------------------------*
       exe-exp-ocv-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "ocv "               to   f-xxx-nam              .
           move      "std_orc_ocv"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-ocv-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-ocv-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-ocv-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-ocv-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-ocv-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-ocv-999.
       exe-exp-ocv-100.
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      rr-dat-exp           to   rf-oct-dat-doc         .
           move      zero                 to   rf-oct-cod-dpz         .
           move      zero                 to   rf-oct-num-doc         .
           move      spaces               to   rf-oct-tmo-orc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocv-800.
       exe-exp-ocv-200.
      *              *-------------------------------------------------*
      *              * Next su [oct]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocv-800.
       exe-exp-ocv-300.
      *              *-------------------------------------------------*
      *              * Max su [oct]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocv-400.
      *              *-------------------------------------------------*
      *              * Sel su [oct]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocv-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-ocv-800.
       exe-exp-ocv-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-ocv-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-ocv-620.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    6
                     go to exe-exp-ocv-750.
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        rf-oct-voc-des
                    (w-exe-ctr-001)       =    spaces
                     go to exe-exp-ocv-620.
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * Protocollo                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-oct-num-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo                                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-ctr-001        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice voce                                 *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-oct-voc-des
                    (w-exe-ctr-001)       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Af"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Composizione carattere di fine record       *
      *                  *---------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti           *
      *                  *---------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocv-620.
       exe-exp-ocv-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [oct]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-ocv-200.
       exe-exp-ocv-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
       exe-exp-ocv-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-ocv-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-ocv-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [ocx]                                        *
      *    *-----------------------------------------------------------*
       exe-exp-ocx-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "ocx "               to   f-xxx-nam              .
           move      "std_orc_ocx"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-ocx-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-ocx-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-ocx-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-ocx-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [ocx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-ocx-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-ocx-999.
       exe-exp-ocx-100.
      *              *-------------------------------------------------*
      *              * Start su file [ocx]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      zero                 to   rf-ocx-num-prt         .
           move      zero                 to   rf-ocx-num-prg         .
           move      zero                 to   rf-ocx-tip-rec         .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocx-800.
       exe-exp-ocx-200.
      *              *-------------------------------------------------*
      *              * Next su [ocx]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-ocx-800.
       exe-exp-ocx-300.
      *              *-------------------------------------------------*
      *              * Max su [ocx]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocx-400.
      *              *-------------------------------------------------*
      *              * Sel su [ocx]                                    *
      *              *-------------------------------------------------*
       exe-exp-ocx-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-ocx-800.
       exe-exp-ocx-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-ocx-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * Protocollo                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocx-num-prt       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo                                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocx-num-prg       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-ocx-tip-rec       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Testo di estensione                         *
      *                  * ___ VALUTARE UN UNICO TESTO ___             *
      *                  *---------------------------------------------*
                      
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
       exe-exp-ocx-620.
           add       1                    to   w-exe-ctr-001          .
           if        w-exe-ctr-001        >    10
                     go to exe-exp-ocx-628.
      *                  *---------------------------------------------*
      *                  * Riga da 40 caratteri                        *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-ocx-rig-400
                    (w-exe-ctr-001)       to   w-scr-str-ele          .
           move      40                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           if        w-exe-ctr-001        =    10
                     move  "Af"           to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-exp-ocx-620.
       exe-exp-ocx-628.
       exe-exp-ocx-690.
      *              *-------------------------------------------------*
      *              * Composizione carattere di fine record           *
      *              *-------------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-ocx-700.
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-exp-ocx-720.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-exp-ocx-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [ocx]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-ocx-200.
       exe-exp-ocx-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [ocx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
       exe-exp-ocx-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-ocx-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-ocx-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [zcv]                                        *
      *    *-----------------------------------------------------------*
       exe-exp-zcv-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "zcv "               to   f-xxx-nam              .
           move      "std_orc_zcv"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-zcv-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-zcv-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-zcv-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-zcv-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [zcv]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzcv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcv                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-zcv-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-zcv-999.
       exe-exp-zcv-100.
      *              *-------------------------------------------------*
      *              * Start su file [zcv]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCDV    "         to   f-key                  .
           move      zero                 to   rf-zcv-cod-cdv         .
           move      "pgm/orc/fls/ioc/obj/iofzcv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcv                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-zcv-800.
       exe-exp-zcv-200.
      *              *-------------------------------------------------*
      *              * Next su [zcv]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzcv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcv                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-zcv-800.
       exe-exp-zcv-300.
      *              *-------------------------------------------------*
      *              * Max su [zcv]                                    *
      *              *-------------------------------------------------*
       exe-exp-zcv-400.
      *              *-------------------------------------------------*
      *              * Sel su [zcv]                                    *
      *              *-------------------------------------------------*
       exe-exp-zcv-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-zcv-800.
       exe-exp-zcv-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-zcv-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zcv-cod-cdv       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zcv-des-cdv       to   w-scr-str-ele          .
           move      240                  to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione in uppercase                    *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zcv-des-key       to   w-scr-str-ele          .
           move      30                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zcv-mne-cdv       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Af"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-zcv-690.
      *              *-------------------------------------------------*
      *              * Composizione carattere di fine record           *
      *              *-------------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-zcv-700.
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-exp-zcv-720.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-exp-zcv-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [zcv]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-zcv-200.
       exe-exp-zcv-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [zcv]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzcv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcv                 .
       exe-exp-zcv-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-zcv-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-zcv-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [zoc]                                        *
      *    *-----------------------------------------------------------*
       exe-exp-zoc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "zoc "               to   f-xxx-nam              .
           move      "std_orc_zoc"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-zoc-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-zoc-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-zoc-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-zoc-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [zoc]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-zoc-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-zoc-999.
       exe-exp-zoc-100.
      *              *-------------------------------------------------*
      *              * Start su file [zoc]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODTOC    "         to   f-key                  .
           move      spaces               to   rf-zoc-cod-toc         .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-zoc-800.
       exe-exp-zoc-200.
      *              *-------------------------------------------------*
      *              * Next su [zoc]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-zoc-800.
       exe-exp-zoc-300.
      *              *-------------------------------------------------*
      *              * Max su [zoc]                                    *
      *              *-------------------------------------------------*
       exe-exp-zoc-400.
      *              *-------------------------------------------------*
      *              * Sel su [zoc]                                    *
      *              *-------------------------------------------------*
       exe-exp-zoc-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-zoc-800.
       exe-exp-zoc-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-zoc-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "iA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-cod-toc       to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione in uppercase                    *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-des-key       to   w-scr-str-ele          .
           move      30                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-des-toc       to   w-scr-str-ele          .
           move      30                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Password                                    *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-pwd-toc       to   w-scr-str-ele          .
           move      08                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Validita' dipendenze                        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zoc-vld-dpz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zoc-cod-dpz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo origine del documento                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zoc-org-doc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo provenienza del documento              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zoc-prv-doc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Sigla numerazione                           *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-sgl-num       to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione per la stampa                   *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-des-stp       to   w-scr-str-ele          .
           move      25                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/No stampa prezzo nel documento           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zoc-snx-prz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/No stampa sconti                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zoc-snx-sco       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/No stampa data consegna                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zoc-snx-dtc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/No stampa totale ordine                  *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-snx-sto       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Default per accettazione tipo riga corpo    *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-def-tpr       to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/No stampa riferimento all'Agente         *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-snx-age       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo ordine                                 *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-tip-ord       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-def-tar       to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Si/No stampa dicitura - SQL                 *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione dicitura - SQL                  *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Default tipo vendita per l'agente - SQL     *
      *                  *---------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Default modulo da stampare - SQL            *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Status                                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zoc-sta-tus       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data status                                 *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      rf-zoc-sta-tud       to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice di riferimento                       *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zoc-sta-tuc       to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento statistiche                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zoc-sta-tux       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data ultima modifica - SQL                  *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      w-exe-dat-exe        to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Utente ultima modifica - SQL                *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      f-xxx-usr            to   w-scr-str-ele          .
           move      08                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Fase ultima modifica - SQL                  *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      i-ide-fas            to   w-scr-str-ele          .
           move      06                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Af"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-zoc-690.
      *              *-------------------------------------------------*
      *              * Composizione carattere di fine record           *
      *              *-------------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-zoc-700.
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-exp-zoc-720.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-exp-zoc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [zoc]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-zoc-200.
       exe-exp-zoc-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [zoc]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
       exe-exp-zoc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-zoc-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-zoc-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [zro]                                        *
      *    *-----------------------------------------------------------*
       exe-exp-zro-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-are              .
           move      "zro "               to   f-xxx-nam              .
           move      "std_orc_zro"        to   f-xxx-npe              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-exp-zro-999.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to exe-exp-zro-010.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-zro-010.
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-exp-zro-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [zro]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzro"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zro                 .
      *                  *---------------------------------------------*
      *                  * Open generica file sequenziale di output    *
      *                  *---------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to exe-exp-zro-100.
      *                  *---------------------------------------------*
      *                  * Se errori                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     exe-exp-zro-999.
       exe-exp-zro-100.
      *              *-------------------------------------------------*
      *              * Start su file [zro]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCDV    "         to   f-key                  .
           move      zero                 to   rf-zro-cod-rsp         .
           move      "pgm/orc/fls/ioc/obj/iofzro"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zro                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-zro-800.
       exe-exp-zro-200.
      *              *-------------------------------------------------*
      *              * Next su [zro]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzro"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zro                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-exp-zro-800.
       exe-exp-zro-300.
      *              *-------------------------------------------------*
      *              * Max su [zro]                                    *
      *              *-------------------------------------------------*
       exe-exp-zro-400.
      *              *-------------------------------------------------*
      *              * Sel su [zro]                                    *
      *              *-------------------------------------------------*
       exe-exp-zro-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
           if        w-exe-flg-brk        not  = spaces
                     go to exe-exp-zro-800.
       exe-exp-zro-550.
      *              *-------------------------------------------------*
      *              * Pulizia preliminare record sequenziale          *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-exp-zro-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-zro-cod-rsp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zro-des-rsp       to   w-scr-str-ele          .
           move      40                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione in uppercase                    *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zro-des-key       to   w-scr-str-ele          .
           move      30                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      rf-zro-cod-mne       to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Reparto - SQL                               *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Pathname immagine - SQL                     *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Af"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-zro-690.
      *              *-------------------------------------------------*
      *              * Composizione carattere di fine record           *
      *              *-------------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-zro-700.
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-exp-zro-720.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-exp-zro-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [zro]                         *
      *              *-------------------------------------------------*
           go to     exe-exp-zro-200.
       exe-exp-zro-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura del file in output                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *                  *---------------------------------------------*
      *                  * [zro]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzro"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zro                 .
       exe-exp-zro-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-exp-zro-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-exp-zro-999.
           exit.

