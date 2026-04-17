       Identification Division.
       Program-Id.                                 provis01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    provis              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/01/00    *
      *                       Ultima revisione:    NdK del 10/04/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Lista prodotti per Browser Internet         *
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
       
      *================================================================*
       Input-Output Section.
      *================================================================*
      
       File-Control.
       
      *    *===========================================================*
      *    * File Control [fil]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fil   assign to disk           f-fil-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is fil-k01
                   alternate record key   is fil-k02
                   alternate record key   is fil-k03
                   alternate record key   is fil-k04
                   alternate record key   is fil-k05
                   alternate record key   is fil-k06
                   alternate record key   is fil-k07
                   alternate record key   is fil-k08
                             file status  is                f-fil-sts .
                             
      ******************************************************************
       Data Division.
      ******************************************************************
      
       File Section.
       
      *    *===========================================================*
      *    * File Description [fil]                                    *
      *    *-----------------------------------------------------------*
       fd  fil       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  fil-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  fil-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-des-key        pic  x(40)                  .
                   15  fil-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ALFPRO                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-alf-pro        pic  x(14)                  .
                   15  fil-num-pro-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : SYNPRO                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-syn-pro        pic  x(13)                  .
                   15  fil-num-pro-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CGSDES                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cla-pro        pic  9(05)       comp-3     .
                   15  fil-gru-pro        pic  9(05)       comp-3     .
                   15  fil-sgr-pro        pic  9(05)       comp-3     .
                   15  fil-des-key-6      pic  x(40)                  .
                   15  fil-num-pro-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CGSALF                         *
      *            *---------------------------------------------------*
               10  fil-k07.
                   15  fil-cla-pro-7      pic  9(05)       comp-3     .
                   15  fil-gru-pro-7      pic  9(05)       comp-3     .
                   15  fil-sgr-pro-7      pic  9(05)       comp-3     .
                   15  fil-alf-pro-7      pic  x(14)                  .
                   15  fil-num-pro-7      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : KLBPRO                         *
      *            *---------------------------------------------------*
               10  fil-k08.
                   15  fil-klb-pro        pic  x(13)                  .
                   15  fil-num-pro-8      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-inf-gen.
                   15  fil-ide-ute        pic  x(08)                  .
                   15  fil-ide-fas        pic  x(06)                  .
                   15  fil-des-pro        pic  x(40)                  .
                   15  fil-des-pdx        pic  9(01)                  .
                   15  fil-tip-pro        pic  9(02)                  .
                   15  fil-tip-acp        pic  9(02)                  .
                   15  fil-not-g01        pic  x(40)                  .
               10  fil-inf-fis.
                   15  fil-tip-cfz        pic  9(02)                  .
                   15  fil-qta-cfz        pic  9(06)v9(03) comp-3     .
                   15  fil-pes-uni        pic  9(06)v9(03) comp-3     .
                   15  fil-pes-tar        pic  9(06)v9(03) comp-3     .
                   15  fil-vol-uni        pic  9(06)v9(03) comp-3     .
                   15  fil-dim-pro.
                       20  fil-dim-lar    pic  9(06)v9(03) comp-3     .
                       20  fil-dim-alt    pic  9(06)v9(03) comp-3     .
                       20  fil-dim-prf    pic  9(06)v9(03) comp-3     .
                   15  fil-pcl-fis        pic  x(10)                  .
                   15  fil-coe-mol        pic  9(04)v9(03) comp-3     .
                   15  fil-coe-div        pic  9(04)v9(03) comp-3     .
                   15  fil-spc-lib        pic  x(20)                  .
               10  fil-inf-fat.
                   15  fil-cod-iva        pic  9(05)       comp-3     .
                   15  fil-ctp-ven        pic  9(07)       comp-3     .
                   15  fil-umi-ven        pic  x(03)                  .
                   15  fil-dec-qta        pic  9(01)                  .
                   15  fil-sgl-vlt        pic  x(03)                  .
                   15  fil-dec-vlt        pic  9(01)                  .
                   15  fil-dec-prz        pic  9(01)                  .
                   15  fil-prz-lst        pic  9(09)       comp-3     .
                   15  fil-lot-ven        pic  9(06)v9(03) comp-3     .
                   15  fil-epz-rgf        pic  9(01)                  .
                   15  fil-snx-2qt        pic  9(01)                  .
                   15  fil-dec-2qt        pic  9(01)                  .
                   15  fil-snx-3qt        pic  9(01)                  .
                   15  fil-dec-3qt        pic  9(01)                  .
                   15  fil-snx-2pz        pic  9(01)                  .
                   15  fil-tip-vve        pic  x(03)                  .
               10  fil-inf-cdv.
                   15  fil-cat-scr        pic  9(05)       comp-3     .
                   15  fil-per-scr occurs 05
                                          pic  9(02)v9(01) comp-3     .
               10  fil-inf-gag.
                   15  fil-cat-pvg        pic  9(05)       comp-3     .
                   15  fil-per-pvg  occurs 03
                                          pic  9(02)v9(01) comp-3     .
                   15  fil-amm-pvg        pic  9(09)       comp-3     .
               10  fil-inf-lst.
                   15  fil-snx-lst        pic  9(01)                  .
                   15  fil-epz-lst.
                       20  fil-epz-ele occurs 20
                                          pic  x(01)                  .
                   15  fil-pag-lst        pic  x(10)                  .
                   15  fil-rfl-lst        pic  x(12)                  .
                   15  fil-tmc-lst        pic  9(03)       comp-3     .
                   15  fil-daa-lst        pic  9(01)                  .
                   15  fil-aut-lst        pic  x(03)                  .
               10  fil-inf-pcs.
                   15  fil-cod-s01        pic  9(05)       comp-3     .
                   15  fil-cod-s02        pic  9(05)       comp-3     .
                   15  fil-cod-s03        pic  9(05)       comp-3     .
                   15  fil-dat-icm        pic  9(07)       comp-3     .
                   15  fil-sta-tus        pic  9(02)                  .
                   15  fil-sta-tud        pic  9(07)       comp-3     .
                   15  fil-sta-tuc        pic  9(07)       comp-3     .
                   15  fil-sta-tux        pic  9(02)                  .
               10  fil-inf-mkt.
                   15  fil-cld-imp        pic  x(01)                  .
                   15  fil-pre-ctn        pic  9(02)                  .
                   15  fil-gra-ico        pic  9(02)                  .
                   15  fil-pcl-ccz        pic  9(02)                  .
                   15  fil-cod-mkt        pic  9(05)       comp-3     .
                   15  fil-ind-mkt        pic  x(10)                  .
               10  fil-inf-bdg.
                   15  fil-cla-bdg        pic  9(05)       comp-3     .
               10  fil-inf-bol.
                   15  fil-for-blo        pic  9(02)                  .
                   15  fil-dor-blo        pic  9(07)       comp-3     .
                   15  fil-fco-blo        pic  9(02)                  .
                   15  fil-dco-blo        pic  9(07)       comp-3     .
               10  fil-inf-iic.
                   15  fil-cdn-cdm        pic  9(08)                  .
               10  fil-inf-aps.
                   15  fil-alx-exp.
                       20  filler occurs 80
                                          pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*
      
      *    *===========================================================*
      *    * File area per [kk0]                                       *
      *    *-----------------------------------------------------------*
       01  f-fil.
           05  f-fil-nam                  pic  x(04)                  .
           05  f-fil-pat                  pic  x(40)                  .
           05  f-fil-sts                  pic  x(02)                  .
           
      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  comodo.
      *        *-------------------------------------------------------*
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  stringa                    pic  x(512)                 .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  conta                      pic  9(04)                  .
           05  contax                     pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per parametri in input                         *
      *        *-------------------------------------------------------*
           05  parametro1                 pic  x(30)                  .
           05  parametro2                 pic  x(30)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di comodo                                     *
      *        *-------------------------------------------------------*
           05  stringa2                   pic  x(40)                  .

      *    *===========================================================*
      *    * Area di chaining                                          *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
       01  parametro                      pic  x(60)                  .

      ******************************************************************
       Procedure Division             chaining parametro              .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   contax                 .
           move      spaces               to   parametro1             .
           move      spaces               to   parametro2             .
      *              *-------------------------------------------------*
      *              * Estrazione dei parametri 'post'                 *
      *              *-------------------------------------------------*
           unstring  parametro
                                delimited by "="
                                          into parametro1
                                    count in   contax                 .
           add       2                    to   contax                 .
           unstring  parametro            into parametro2
                                  with pointer contax                 .
      *              *-------------------------------------------------*
      *              * Apertura file prodotti                          *
      *              *-------------------------------------------------*
           move      "/abd/azi/zat/dcp"
                                          to   f-fil-pat              .
           open      i-o    fil                                       .
       main-100.
      *              *-------------------------------------------------*
      *              * Ordinamento per codice                          *
      *              *-------------------------------------------------*
           move      parametro2 (01 : 14) to   fil-alf-pro            .
           move      zero                 to   fil-num-pro-4          .
           start     fil    key not less
                            fil-k04
                            invalid key
                            go to main-900.
           go to     main-200.
       main-200.
      *              *-------------------------------------------------*
      *              * Read next                                       *
      *              *-------------------------------------------------*
           add       1                    to   conta                  .
       main-202.
           read      fil    next
                            with no lock
                            at end
                            go to main-900.
       main-203.
           if        fil-alf-pro          not  = parametro2
                                                (01 : 14)
                     go to main-900.
       main-205.
           if        fil-sta-tus          not  = 01
                     go to main-200.
           if        conta = 1
                     go to main-210
           else      go to main-220.
       main-210.
      *              *-------------------------------------------------*
      *              * Se primo record in assoluto                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Testata file HTML                           *
      *                  *---------------------------------------------*
           display "Content-type: text/html" with no advancing.
           display "".
           display "<head>".
           display "<title>Nicola de Kunovich</title>".
           display "</head>".
           display "<body>".
           display "<body bgcolor=#ffffff TEXT=#000000>"              .
           display "<A NAME=inizio>".
           display "<hr size=2>"
           display "<h3><center>TANGRAM : Lista prodotti</center></h3>".
           display "<hr size=2>"
      *                  *---------------------------------------------*
      *                  * Tasto di rientro                            *
      *                  *---------------------------------------------*
           display "<a HREF=/index.html><img src=/icons/back.gif
      -              "></a>"                        .
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento                            *
      *                  *---------------------------------------------*
           move   spaces to stringa.
           string "<h3>" delimited by size
                  parametro1
                         delimited by size
                  "</h3>"
                         delimited by size
                                   into stringa                       .
           display   stringa                                          .
      *                  *---------------------------------------------*
      *                  * Separatore                                  *
      *                  *---------------------------------------------*
           display "<hr size=3>"
      *                  *---------------------------------------------*
      *                  * Secondo parametro                           *
      *                  *---------------------------------------------*
           move spaces to stringa.
           string "<h3>" delimited by size
                  parametro2
                         delimited by size
                  "</h3>"
                         delimited by size
                                   into stringa                       .
           display   stringa                                          .
      *                  *---------------------------------------------*
      *                  * Fincatura tabella                           *
      *                  *---------------------------------------------*
           display "<table border=1>".
           display "<caption>".
           display "Articoli".
           display "</caption>".
           display "<tr align=center>".
           display "<th>nr</th>".
           display "<th>Codice</th>".
           display "<th>Descrizione</th>".
           display "<th>Foto</th>".
           display "</tr>".
       main-220.
      *              *-------------------------------------------------*
      *              * Se 'n' record                                   *
      *              *-------------------------------------------------*
           if        conta = 30
                     go to main-900.
      *                  *---------------------------------------------*
      *                  * Normalizzazione eventuale                   *
      *                  *---------------------------------------------*
           if        fil-rfl-lst          =    spaces
                     move  "000000"       to   fil-rfl-lst            .
           move spaces to stringa2.
           string "../jpg/" 
                                delimited by size
                  fil-rfl-lst
                    (01 : 02)   delimited by spaces
                  "/"           delimited by size
                  fil-rfl-lst   delimited by spaces
                                          into stringa2               .
      *                  *---------------------------------------------*
      *                  * Elemento in tabella                         *
      *                  *---------------------------------------------*
           display "<tr align=center>".
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare stringa     *
      *                      *-----------------------------------------*
           move spaces to stringa.
           string "<td>" delimited by size
      *                      *-----------------------------------------*
      *                      * Numero record                           *
      *                      *-----------------------------------------*
                  conta
                         delimited by size
                  "</td><td>"
                         delimited by size
      *                      *-----------------------------------------*
      *                      * Codice prodotto                         *
      *                      *-----------------------------------------*
                  fil-alf-pro
                         delimited by size
                  "</td><td>"
                         delimited by size
      *                      *-----------------------------------------*
      *                      * Descrizione prodotto                    *
      *                      *-----------------------------------------*
                  fil-des-pro
                         delimited by size
                  "</td><td>"
                         delimited by size
      *                      *-----------------------------------------*
      *                      * Immagine                                *
      *                      *-----------------------------------------*
______*           "<A HREF="
______*                  delimited by size
______*           stringa2
______*                  delimited by spaces
______*           ".jpg><img src="
                  "<img src="
                         delimited by size
                  stringa2
                         delimited by spaces
                  ".jpg border=0 width=300>"
                         delimited by size
                  "</td>"
                         delimited by size
                                   into stringa                       .
           display   stringa                                          .
           display "</tr>".
      *              *-------------------------------------------------*
      *              * Riciclo a read-next                             *
      *              *-------------------------------------------------*
           go to main-200.
       main-900.
      *              *-------------------------------------------------*
      *              * Fine HTML                                       *
      *              *-------------------------------------------------*
           display "</table>".
           display "<a HREF=#inizio><img src=/icons/up.gif></a>"      .
           display "</body>".
           display "</html>".
      *              *-------------------------------------------------*
      *              * Close file [dcp]                                *
      *              *-------------------------------------------------*
           close     fil                                              .
       main-999.
           exit      program.
           stop run.

