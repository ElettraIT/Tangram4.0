      *    *===========================================================*
      *    * Emissione HTML                                            *
      *    *-----------------------------------------------------------*
       emi-htm-000.
      *              *-------------------------------------------------*
      *              * Test se elemento speciale                       *
      *              *-------------------------------------------------*
           if        w-htm-tip-tag        =    "title"
                     perform emi-htm-tit-000
                                          thru emi-htm-tit-999
                     go to emi-htm-900
           else if   w-htm-tip-tag        =    "docum"
                     perform emi-htm-doc-000
                                          thru emi-htm-doc-999
                     go to emi-htm-900
           else      go to emi-htm-050.
       emi-htm-050.
      *              *-------------------------------------------------*
      *              * Test se elemento semplice o con opzioni         *
      *              *-------------------------------------------------*
           if        w-htm-tip-ope        not  = "A" and
                     w-htm-tip-ope        not  = "C"
                     go to emi-htm-200.
       emi-htm-100.
      *              *-------------------------------------------------*
      *              * Tag semplice                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A trattamento elemento semplice             *
      *                  *---------------------------------------------*
           perform   emi-htm-sop-000      thru emi-htm-sop-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     emi-htm-900.
       emi-htm-200.
      *              *-------------------------------------------------*
      *              * Tag con opzioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A trattamento elemento semplice             *
      *                  *---------------------------------------------*
           perform   emi-htm-cop-000      thru emi-htm-cop-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     emi-htm-900.
       emi-htm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-999.
       emi-htm-999.
           exit.

      *    *===========================================================*
      *    * Emissione HTML                                            *
      *    *                                                           *
      *    * Elemento speciale : apertura documento                    *
      *    *-----------------------------------------------------------*
       emi-htm-doc-000.
      *              *-------------------------------------------------*
      *              * Intestazione                                    *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
       emi-htm-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-doc-999.
       emi-htm-doc-999.
           exit.

      *    *===========================================================*
      *    * Emissione HTML                                            *
      *    *                                                           *
      *    * Elemento speciale : title                                 *
      *    *-----------------------------------------------------------*
       emi-htm-tit-000.
      *              *-------------------------------------------------*
      *              * Commento                                        *
      *              *-------------------------------------------------*
           display   "<!-- TITOLO -->"                                .
      *
           move      220                  to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<title>"            to   w-all-str-cat (1)      .
           move      w-htm-opz-val (01)   to   w-all-str-cat (2)      .
           move      "</title>"           to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           display   w-all-str-alf                                    .
       emi-htm-tit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-tit-999.
       emi-htm-tit-999.
           exit.

      *    *===========================================================*
      *    * Emissione HTML                                            *
      *    *                                                           *
      *    * Elemento senza opzioni                                    *
      *    *-----------------------------------------------------------*
       emi-htm-sop-000.
      *              *-------------------------------------------------*
      *              * Commento                                        *
      *              *-------------------------------------------------*
           move      220                  to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
      *
           move      "<!--"               to   w-all-str-cat (1)      .
           move      w-htm-nom-ele        to   w-all-str-cat (2)      .
      *
           if        w-htm-tip-ope        =    "A"
                     move  spaces         to   w-all-str-cat (3)
           else      move  "* FINE"       to   w-all-str-cat (3)      .
           move      "* -->"              to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           display   w-all-str-alf                                    .
      *              *-------------------------------------------------*
      *              * Tag                                             *
      *              *-------------------------------------------------*
           move      220                  to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
      *
           move      "<"                  to   w-all-str-cat (1)      .
      *
           if        w-htm-tip-ope        =    "A"
                     move  spaces         to   w-all-str-cat (2)
           else      move  "/"            to   w-all-str-cat (2)      .
      *
           move      w-htm-tip-tag        to   w-all-str-cat (3)      .
           move      ">"                  to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           display   w-all-str-alf                                    .
       emi-htm-sop-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-sop-999.
       emi-htm-sop-999.
           exit.

      *    *===========================================================*
      *    * Emissione HTML                                            *
      *    *                                                           *
      *    * Elemento con opzioni                                      *
      *    *-----------------------------------------------------------*
       emi-htm-cop-000.
      *              *-------------------------------------------------*
      *              * Commento                                        *
      *              *-------------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      01                   to   v-car                  .
           move      w-htm-tip-ope        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-htm-opz-num          .
       emi-htm-cop-100.
      *              *-------------------------------------------------*
      *              * Elemento con opzioni                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo per 'n' opzioni                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-htm-opz-ctr          .
       emi-htm-cop-200.
           add       1                    to   w-htm-opz-ctr          .
           if        w-htm-opz-ctr        >    w-htm-opz-num
                     go to emi-htm-cop-800.
           if        w-htm-opz-ctr        >    w-htm-opz-max
                     go to emi-htm-cop-800.
      *                  *---------------------------------------------*
      *                  * Apertura tag                                *
      *                  *---------------------------------------------*
           if        w-htm-opz-ctr        >    1
                     go to emi-htm-cop-400.
      *                  *---------------------------------------------*
      *                  * Commento                                    *
      *                  *---------------------------------------------*
           move      220                  to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
      *
           move      "<!-- *"             to   w-all-str-cat (1)      .
           move      w-htm-nom-ele        to   w-all-str-cat (2)      .
           move      "* -->"              to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           display   w-all-str-alf                                    .
      *                  *---------------------------------------------*
      *                  * Tag                                         *
      *                  *---------------------------------------------*
           move      220                  to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
      *
           move      "<"                  to   w-all-str-cat (1)      .
           move      w-htm-tip-tag        to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           display   w-all-str-alf                                    .
       emi-htm-cop-400.
      *                  *---------------------------------------------*
      *                  * Opzioni tag                                 *
      *                  *---------------------------------------------*
           move      220                  to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
      *
           move      w-htm-opz-nom
                    (w-htm-opz-ctr)       to   w-all-str-cat (1)      .
           move      "= '"                to   w-all-str-cat (2)      .
           move      w-htm-opz-val
                    (w-htm-opz-ctr)       to   w-all-str-cat (3)      .
           move      "'"                  to   w-all-str-cat (4)      .
      *
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           display   w-all-str-alf                                    .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     emi-htm-cop-200.
       emi-htm-cop-800.
      *                  *---------------------------------------------*
      *                  * Chiusura                                    *
      *                  *---------------------------------------------*
           display   "/>"                                             .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     emi-htm-cop-900.
       emi-htm-cop-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-cop-999.
       emi-htm-cop-999.
           exit.

