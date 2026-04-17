      *    *===========================================================*
      *    * Routine di interfaccia per scrittura e lettura 'mysql'    *
      *    *                                                           *
      *    * ___ IN LAVORAZIONE ___                                    *
      *    *                                                           *
      *    * ___ VALUTARE ALTERNATIVA perl ___                         *
      *    *-----------------------------------------------------------*
       int-msq-ope-000.
      *              *-------------------------------------------------*
      *              * Test se eseguibile                              *
      *              *-------------------------------------------------*
           if        w-int-msq-pwd        =    spaces
                     go to int-msq-ope-900.
       int-msq-ope-100.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       int-msq-ope-150.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-int-msq-ope        =    "IN"
                     go to int-msq-ope-200
           else if   w-int-msq-ope        =    "SE"
                     go to int-msq-ope-400
           else if   w-int-msq-ope        =    "UP"
                     go to int-msq-ope-600
           else if   w-int-msq-ope        =    "DE"
                     go to int-msq-ope-700
           else      go to int-msq-ope-850.


       int-msq-ope-200.
      *              *=================================================*
      *              * INSERT su database                              *
      *              *-------------------------------------------------*




       int-msq-ope-400.
      *              *=================================================*
      *              * SELECT su database                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Assemblaggio comando                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-int-msq-cmd          .
      *
           string    "mysql -u "
                                delimited by   size
                     w-int-msq-usr
                                delimited by   spaces
                     " -p"      delimited by   size
                     w-int-msq-pwd
                                delimited by   spaces
                     " -N "     delimited by   size
                     w-int-msq-dtb
                                delimited by   spaces
                     " -e ""SELECT "
                                delimited by   size
                     "nam_xml from xml_for WHERE dat_tgm = 0"" > "
                                delimited by   size
                     w-det-pth-fdc-ptd
                                delimited by   spaces
                     "/"        delimited by   size
                     w-det-pth-fdc-ptn
                                delimited by   spaces
                                          into o-shs                  .







       int-msq-ope-600.
      *              *=================================================*
      *              * UPDATE su database                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Assemblaggio comando                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-int-msq-cmd          .
      *
           string    "mysql -u "
                                delimited by   size
                     w-int-msq-usr
                                delimited by   spaces
                     " -p"      delimited by   size
                     w-int-msq-pwd
                                delimited by   spaces
                     " -N "     delimited by   size
                     w-int-msq-dtb
                                delimited by   spaces
                     " -e ""UPDATE "
                                delimited by   size
                     w-int-msq-tab
                                delimited by   spaces
                     " SET "    delimited by   size
                     w-int-msq-fld
                                delimited by   spaces
                     " = MD5('" delimited by   size
                     w-int-msq-upw
                                delimited by   spaces
                     "') , "    delimited by   size
                     w-int-msq-dag
                                delimited by   spaces
                     " = NOW() WHERE "
                                delimited by   size
                     w-int-msq-flk
                                delimited by   spaces
                     " = '"     delimited by   size
                     w-int-msq-ute
                                delimited by   spaces
                     "' LIMIT 1"""
                                delimited by   size
                                          into w-int-msq-cmd          .
      *                  *---------------------------------------------*
      *                  * Ad esecuzione comando                       *
      *                  *---------------------------------------------*
           go to int-msq-ope-800.
      


       int-msq-ope-750.
      *              *=================================================*
      *              * DELETE su database                              *
      *              *-------------------------------------------------*



      
       int-msq-ope-800.
      *              *-------------------------------------------------*
      *              * Richiamo del modulo 'mopsys'                    *
      *              *-------------------------------------------------*
           move      "SH"                 to   o-ope                  .
           move      w-int-msq-cmd        to   o-shs                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       int-msq-ope-850.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       int-msq-ope-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     int-msq-ope-999.
       int-msq-ope-999.
           exit.


