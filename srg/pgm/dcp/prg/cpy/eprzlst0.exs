      *    *===========================================================*
      *    * Subroutines per l'espansione del prezzo di listino        *
      *    *-----------------------------------------------------------*
      *              *-------------------------------------------------*
      *              * Open                                            *
      *              *-------------------------------------------------*
       xpd-prz-lst-opn-000.
           move      "pgm/dcp/prg/obj/eprzlst0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   w-xpd-prz-lst-pth      .
           move      "OP"                 to   w-xpd-prz-lst-ope      .
           perform   xpd-prz-lst-cll-000  thru xpd-prz-lst-cll-999    .
       xpd-prz-lst-opn-999.
           exit.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
       xpd-prz-lst-cls-000.
           move      "CL"                 to   w-xpd-prz-lst-ope      .
           perform   xpd-prz-lst-cll-000  thru xpd-prz-lst-cll-999    .
           move      "C?"                 to   w-xpd-prz-lst-ope      .
           perform   xpd-prz-lst-cll-000  thru xpd-prz-lst-cll-999    .
           if        w-xpd-prz-lst-ope    not  = spaces
                     go to xpd-prz-lst-cls-999.
           cancel    w-xpd-prz-lst-pth                                .
       xpd-prz-lst-cls-999.
           exit.
      *              *-------------------------------------------------*
      *              * Call                                            *
      *              *-------------------------------------------------*
       xpd-prz-lst-cll-000.
           call      w-xpd-prz-lst-pth   using w-xpd-prz-lst
                                               v
                                               s                      .
       xpd-prz-lst-cll-999.
           exit.
