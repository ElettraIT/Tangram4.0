      *    *************************************************************
      *    * Raccolta di routines di trattamento stringhe              *
      *    *                                                           *
      *    * N.B.: tratto da 'wallstr0' - stringhe grandi              *
      *    * ========================================================= *
      *    *                                                           *
      *    * big-str-asx-000/999 : allineamento stringa a  Sinistra    *
      *    *                                                           *
      *    * ========================================================= *
      *    *                                                           *
      *    * big-str-cat-000/999 : concatenamento di max 10 stringhe   *
      *    *                                                           *
      *    * ========================================================= *
      *    *                                                           *
      *    * big-str-lun-000/999 : determinazione lunghezza stringa    *
      *    *                                                           *
      *    *                                                           *
      *    * big-str-ext-000/999 : estrazione di max 10 stringhe       *
      *    *                       con delimitatore                    *
      *    *                                                           *
      *    *************************************************************

      *    *===========================================================*
      *    * Routine per il concatenamento di max 10 stringhe di max   *
      *    * 800 caratteri ciascuna senza lasciare alcuno spazio di    *
      *    * separazione tra una stringa e l'altra                     *
      *    *                                                           *
      *    * N.B.: tratto da 'wallstr0'                                *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    *                                                           *
      *    * Input  : w-big-str-lun     = Lunghezza massima della      *
      *    *                              stringa concatenata          *
      *    *                                                           *
      *    *          w-big-str-num     = Numero delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    *          w-big-str-cat (i) = Valore delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    * Output : w-big-str-alf     = Valore della stringa con-    *
      *    *                              catenata, allineata a si-    *
      *    *                              nistra                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       big-str-cat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-big-str-alf          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione del numero di strin-  *
      *              * ghe da concatenare                              *
      *              *-------------------------------------------------*
           if        w-big-str-num        >    10
                     move  10             to   w-big-str-num          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa in output                               *
      *              *-------------------------------------------------*
           if        w-big-str-lun        <    001
                     move  001            to   w-big-str-lun          .
           if        w-big-str-lun        >    8000
                     move  8000           to   w-big-str-lun          .
      *              *-------------------------------------------------*
      *              * Se zero stringhe da concatenare : uscita a spa- *
      *              * ces                                             *
      *              *-------------------------------------------------*
           if        w-big-str-num        =    zero
                     go to big-str-cat-999.
       big-str-cat-100.
      *              *-------------------------------------------------*
      *              * Se le stringhe in input sono tutte a spaces :   *
      *              * uscita a spaces                                 *
      *              *-------------------------------------------------*
       big-str-cat-110.
           move      zero                 to   w-big-str-inx          .
       big-str-cat-120.
           add       1                    to   w-big-str-inx          .
           if        w-big-str-inx        >    w-big-str-num
                     go to big-str-cat-999.
           if        w-big-str-cat
                    (w-big-str-inx)       =    spaces
                     go to big-str-cat-120.
       big-str-cat-200.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per esame substringhe   *
      *              * da concatenare                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-big-str-inx          .
      *              *-------------------------------------------------*
      *              * Inizializzazione puntatore su stringa in out-   *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      1                    to   w-big-str-pnt          .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero residuo di caratte- *
      *              * ri residui concatenabili                        *
      *              *-------------------------------------------------*
           move      w-big-str-lun        to   w-big-str-max          .
       big-str-cat-300.
      *              *-------------------------------------------------*
      *              * Incremento indice su substringa                 *
      *              *-------------------------------------------------*
           add       1                    to   w-big-str-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre il numero di stringhe da concatenare : *
      *              * uscita                                          *
      *              *-------------------------------------------------*
           if        w-big-str-inx        >    w-big-str-num
                     go to big-str-cat-999.
      *              *-------------------------------------------------*
      *              * Se substringa a spaces : la si ignora           *
      *              *-------------------------------------------------*
           if        w-big-str-cat
                    (w-big-str-inx)       =    spaces
                     go to big-str-cat-300.
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della substringa                *
      *              *-------------------------------------------------*
           move      zero                 to   w-big-str-i01          .
           inspect   w-big-str-cat
                    (w-big-str-inx)   tallying w-big-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-big-str-i02          .
           inspect   w-big-str-cat
                    (w-big-str-inx)   tallying w-big-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * substringa, troncandola eventualmente alla mas- *
      *              * sima lunghezza residua                          *
      *              *-------------------------------------------------*
           move      800                  to   w-big-str-i03          .
           subtract  w-big-str-i01        from w-big-str-i03          .
           subtract  w-big-str-i02        from w-big-str-i03          .
           if        w-big-str-i03        >    w-big-str-max
                     move  w-big-str-max  to   w-big-str-i03          .
      *              *-------------------------------------------------*
      *              * Concatenazione                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-big-str-i01          .
           move      w-big-str-cat
                    (w-big-str-inx)
                    (w-big-str-i01:
                     w-big-str-i03)       to   w-big-str-alf
                                              (w-big-str-pnt:
                                               w-big-str-i03)         .
      *              *-------------------------------------------------*
      *              * Incremento puntatore su stringa in output       *
      *              *-------------------------------------------------*
           add       w-big-str-i03        to   w-big-str-pnt          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-big-str-pnt        >    w-big-str-lun
                     go to big-str-cat-999.
      *              *-------------------------------------------------*
      *              * Decremento numero residuo di caratteri disponi- *
      *              * bili                                            *
      *              *-------------------------------------------------*
           subtract  w-big-str-i03        from w-big-str-max          .
      *              *-------------------------------------------------*
      *              * Riciclo a substringa successiva                 *
      *              *-------------------------------------------------*
           go to     big-str-cat-300.
       big-str-cat-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione della lunghezza di una      *
      *    * stringa alfanumerica                                      *
      *    *                                                           *
      *    * N.B.: tratto da 'wallstr0'                                *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-big-str-alf     = Stringa da misurare          *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-big-str-lun     = Lunghezza risultante         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       big-str-lun-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-big-str-lun          .
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-big-str-alf        =    spaces
                     go to big-str-lun-999.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-big-str-i01          .
           inspect   w-big-str-alf    tallying w-big-str-i01
                                  for trailing spaces                 .
           move      8000                 to   w-big-str-lun          .
           subtract  w-big-str-i01        from w-big-str-lun          .
       big-str-lun-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'allineamento di una stringa a sinistra      *
      *    *                                                           *
      *    * N.B.: tratto da 'wallstr0'                                *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-big-str-lun = Lunghezza massima della stringa  *
      *    *                                                           *
      *    *          w-big-str-alf = Stringa da allineare             *
      *    *                          re                               *
      *    *                                                           *
      *    * Output : w-big-str-alf = Stringa allineata                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       big-str-asx-000.
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-big-str-alf        =    spaces
                     go to big-str-asx-999.
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa dichiarata                              *
      *              *-------------------------------------------------*
           if        w-big-str-lun        <    001
                     move  001            to   w-big-str-lun          .
           if        w-big-str-lun        >    8000
                     move  8000           to   w-big-str-lun          .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-big-str-i01          .
           inspect   w-big-str-alf    tallying w-big-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-big-str-i02          .
           inspect   w-big-str-alf    tallying w-big-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * stringa in input, troncandola eventualmente al- *
      *              * la massima lunghezza dichiarata                 *
      *              *-------------------------------------------------*
           move      8000                 to   w-big-str-i03          .
           subtract  w-big-str-i01        from w-big-str-i03          .
           subtract  w-big-str-i02        from w-big-str-i03          .
           if        w-big-str-i03        >    w-big-str-lun
                     move  w-big-str-lun  to   w-big-str-i03          .
      *              *-------------------------------------------------*
      *              * Spostamento della stringa utile in input in a-  *
      *              * rea di transito allineata a sinistra            *
      *              *-------------------------------------------------*
           move      spaces               to   w-big-str-wst          .
           add       1                    to   w-big-str-i01          .
           move      w-big-str-alf
                    (w-big-str-i01:
                     w-big-str-i03)       to   w-big-str-wst
                                              (1 : w-big-str-i03)     .
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-big-str-alf          .
      *              *-------------------------------------------------*
      *              * Preparazione stringa in outpiut allineata a si- *
      *              * nistra                                          *
      *              *-------------------------------------------------*
           move      w-big-str-wst
                    (1 : w-big-str-i03)   to   w-big-str-alf
                                              (1 : w-big-str-i03)     .
       big-str-asx-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'estrazione di max 10 stringhe separate da   *
      *    * un delimitatore                                           *
      *    *                                                           *
      *    * N.B.: tratto da 'wallstr0'                                *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-big-str-alf     = Valore della stringa da cui  *
      *    *                              estrarre                     *
      *    *                                                           *
      *    *          w-big-str-del     = Delimitatore                 *
      *    *                                                           *
      *    * Output : w-big-str-cat (i) = Stringhe estratte            *
      *    *                                                           *
      *    *          w-big-str-num     = Numero stringhe estratte     *
      *    *-----------------------------------------------------------*
       big-str-ext-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-big-str-num          .
           move      spaces               to   w-big-str-cat (01)     .
           move      spaces               to   w-big-str-cat (02)     .
           move      spaces               to   w-big-str-cat (03)     .
           move      spaces               to   w-big-str-cat (04)     .
           move      spaces               to   w-big-str-cat (05)     .
           move      spaces               to   w-big-str-cat (06)     .
           move      spaces               to   w-big-str-cat (07)     .
           move      spaces               to   w-big-str-cat (08)     .
           move      spaces               to   w-big-str-cat (09)     .
           move      spaces               to   w-big-str-cat (10)     .
      *              *-------------------------------------------------*
      *              * Test preliminare se il campo e' vuoto           *
      *              *-------------------------------------------------*
           if        w-big-str-alf        =    spaces
                     go to big-str-ext-900.
       big-str-ext-100.
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           unstring  w-big-str-alf
                                delimited by   w-big-str-del
                                          into w-big-str-cat (01)
                                               w-big-str-cat (02)
                                               w-big-str-cat (03)
                                               w-big-str-cat (04)
                                               w-big-str-cat (05)
                                               w-big-str-cat (06)
                                               w-big-str-cat (07)
                                               w-big-str-cat (08)
                                               w-big-str-cat (09)
                                               w-big-str-cat (10)     .
      *              *-------------------------------------------------*
      *              * Ciclo di verifica numero stringhe estratte      *
      *              *-------------------------------------------------*
           move      zero                 to   w-big-str-i01          .
       big-str-ext-200.
           add       1                    to   w-big-str-i01          .
           if        w-big-str-i01        >    10
                     go to big-str-ext-900.
           if        w-big-str-cat
                    (w-all-str-i01)       =    spaces
                     go to big-str-ext-900.
           add       1                    to   w-big-str-num          .
       big-str-ext-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     big-str-ext-200.
       big-str-ext-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     big-str-ext-999.
       big-str-ext-999.
           exit.

