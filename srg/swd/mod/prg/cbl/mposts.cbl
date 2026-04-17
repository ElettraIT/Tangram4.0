       Identification Division.
       Program-Id.                                 mposts             .
      *================================================================*
      *                                                                *
      * Modulo per stampa in PostScript                                *
      *                                                                *
      * Sandro - PERONTECNICI                                          *
      *                                                                *
      * ___ MAI USATO: era specifico per 'porc300t' ___                *
      * ___            vedi 't_ptu_orc_fdl' in 'porc300t'___           *
      * ___            SU MISURA PER PERONTECNICI (pet/orc) ___        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Sequenza obbligatoria di utilizzo :                            *
      *                                                                *
      *  - JobInit                                                     *
      *  - JobStart                                                    *
      *  - JobPrologStart                                              *
      *    .... operazioni per il caricamento di EPS globali eccetera  *
      *  - JobPrologEnd                                                *
      *  - JobSetupStart                                               *
      *    .... operazioni per il setup, come ad esempio lo scaling    *
      *  - JobSetupEnd                                                 *
      *  - PageStart                                                   *
      *    .... operazioni per la costruzione della pagina 1           *
      *  - PageEnd                                                     *
      *    .                                                           *
      *    .                                                           *
      *  - PageStart                                                   *
      *    .... operazioni per la costruzione della pagina n           *
      *  - PageEnd                                                     *
      *  - JobEnd                                                      *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * ps-tip-ope = JobInit                                           *
      *                                                                *
      *          Inizializzazione pre-esecuzione job.                  *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Tutti i campi dell'area di comunicazione ps vengono   *
      *          normalizzati, ad eccezione di ps-tip-ope              *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = JobStart                                          *
      *                                                                *
      *          Inizializzazione del job di stampa PostScript.        *
      *                                                                *
      * Input  : ps-cod-stp = Codice della stampante Tangram su cui    *
      *                       eseguire la stampa, che sara' sempre     *
      *                       in diretta, via spooler, e mai su disco. *
      *                                                                *
      *          ps-pag-siz = Page size               width    height  *
      *                                             --------  -------- *
      *                     - Custom                xxxx,xxx  xxxx,xxx *
      *                     - Slide                  527,760   792,000 *
      *                     - Letter                 612,000   792,000 *
      *                     - Legal                  612,000  1008,000 *
      *                     - Tabloid                792,000  1224,000 *
      *                     - Statement-Half         396,000   612,000 *
      *                     - Executive              522,000   756,000 *
      *                     - Fanfold                792,000  1071,360 *
      *                     - Double                 792,000  1224,000 *
      *                     - Broad-Sheet           1296,000  1728,000 *
      *                     - A0                    2383,937  3370,394 *
      *                     - A1                    1683,780  2383,937 *
      *                     - A2                    1190,551  1683,780 *
      *                     - A3                     841,890  1190,551 *
      *                     - A4                     595,276   841,890 *
      *                     - A5                     419,528   595,276 *
      *                     - A6                     297,638   419,528 *
      *                     - B1-ISO                2004,094  2834,646 *
      *                     - B4-ISO                 708,661  1000,630 *
      *                     - B5-ISO                 498,898   708,661 *
      *                     - B4-JIS                 728,504  1031,811 *
      *                     - B5-JIS                 515,906   728,504 *
      *                     - C3                     918,425  1298,268 *
      *                     - C4                     649,134   918,425 *
      *                     - C5                     459,213   649,134 *
      *                     - C6                     323,150   459,213 *
      *                     - RA2                   1218,898  1729,134 *
      *                     - RA3                    864,567  1218,898 *
      *                     - RA4                    609,449   864,567 *
      *                     - Envelope-#9            639,360   279,360 *
      *                     - Envelope-#10           684,000   297,360 *
      *                     - Envelope-#11           747,360   324,000 *
      *                     - Envelope-#12           792,000   342,000 *
      *                     - Envelope-#14           828,000   360,000 *
      *                     - Envelope-Monarch       540,000   279,360 *
      *                     - Envelope-Check         617,760   279,360 *
      *                     - DL                     623,622   311,811 *
      *                     - German-Fanfold         612,000   864,000 *
      *                     - German-Legal-Fanfold   612,000   936,000 *
      *                                                                *
      *          ps-pag-wid = Page width,  solo se Custom, in Pt       *
      *                                                                *
      *          ps-pag-hei = Page height, solo se Custom, in Pt       *
      *                                                                *
      *          ps-pag-ori = Page orientation                         *
      *                                                                *
      *                     - Portrait                                 *
      *                     - Landscape                                *
      *                                                                *
      * Output : ps-pag-wid = Page width                               *
      *                                                                *
      *          ps-pag-hei = Page height                              *
      *                                                                *
      * Note   : Dopo l'esecuzione dell'operazione saranno attivi i    *
      *          seguenti parametri:                                   *
      *                                                                *
      *          - Horizontal factor : 1                               *
      *          - Vertical factor   : 1                               *
      *          - Line width        : 0.5 Pt                          *
      *          - Dash              : Solid                           *
      *          - Line cap          : Butt                            *
      *          - Line join         : Miter                           *
      *          - Miter limit       : 11 degrees                      *
      *          - Stroke color      : Black                           *
      *          - Fill color        : Black                           *
      *          - Text color        : Black                           *
      *          - Rounding radius   : 5.65714 Pt, ovvero 2 mm         *
      *          - Shading offset    : 4.24285 Pt, ovvero 1,5 mm       *
      *          - Shading black     : 0.50                            *
      *          - Font              : Courier 10                      *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = JobEnd                                            *
      *                                                                *
      *          Termine del job di stampa PostScript, con rilascio    *
      *          alla stampa.                                          *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = JobPrologStart                                    *
      *                                                                *
      *          Dichiarazione di inizio della definizione di procedu- *
      *          re globali, valide per tutte le pagine di stampa.     *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = JobPrologEnd                                      *
      *                                                                *
      *          Dichiarazione di fine della definizione di procedure  *
      *          globali, valide per tutte le pagine di stampa.        *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = JobSetupStart                                     *
      *                                                                *
      *          Dichiarazione di inizio delle operazioni di setup i-  *
      *          niziale, valide per tutte le pagine di stampa, come   *
      *          ad esempio lo scaling generale.                       *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = JobSetupEnd                                       *
      *                                                                *
      *          Dichiarazione di fine delle operazioni di setup ini-  *
      *          ziale.                                                *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = PageStart                                         *
      *                                                                *
      *          Inizio di una pagina di stampa.                       *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = PageEnd                                           *
      *                                                                *
      *          Fine di una pagina di stampa.                         *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SaveGlobal                                        *
      *                                                                *
      *          Salvataggio globale della Virtual Memory e del Gra-   *
      *          phics State.                                          *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = RestoreGlobal                                     *
      *                                                                *
      *          Ripristino globale della Virtual Memory e del Gra-    *
      *          phics State.                                          *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SaveGraphicsState                                 *
      *                                                                *
      *          Salvataggio del Graphics State.                       *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = RestoreGraphicsState                              *
      *                                                                *
      *          Ripristino del Graphics State.                        *
      *                                                                *
      * Input  : Nessuno                                               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetHorizontalFactor                               *
      *                                                                *
      *          Set dell'unita' di misura in cui verranno espresse le *
      *          coordinate di ascissa.                                *
      *                                                                *
      * Input  : ps-lit-scf = Scaling factor espresso come literal     *
      *                                                                *
      *                     - Pt = Points                              *
      *                     - In = Inches                              *
      *                     - Mm = Millimeters                         *
      *                     - Cm = Centimeters                         *
      *                                                                *
      *             oppure                                             *
      *                                                                *
      *          ps-num-scf = Scaling factor espresso come valore      *
      *                       numerico in numero di punti              *
      *                                                                *
      * Output : ps-lit-scf : rinormalizzato a spaces                  *
      *                                                                *
      *          ps-num-scf : rinormalizzato a zero                    *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetVerticalFactor                                 *
      *                                                                *
      *          Set dell'unita' di misura in cui verranno espresse le *
      *          coordinate di ordinata.                               *
      *                                                                *
      * Input  : ps-lit-scf = Scaling factor espresso come literal     *
      *                                                                *
      *                     - Pt = Points                              *
      *                     - In = Inches                              *
      *                     - Mm = Millimeters                         *
      *                     - Cm = Centimeters                         *
      *                                                                *
      *             oppure                                             *
      *                                                                *
      *          ps-num-scf = Scaling factor espresso come valore      *
      *                       numerico in numero di punti              *
      *                                                                *
      * Output : ps-lit-scf : rinormalizzato a spaces                  *
      *                                                                *
      *          ps-num-scf : rinormalizzato a zero                    *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetHorizontalVerticalFactor                       *
      *                                                                *
      *          Set dell'unita' di misura in cui verranno espresse    *
      *          sia le coordinate di ascissa che quelle di ordinata.  *
      *                                                                *
      * Input  : ps-lit-scf = Scaling factor espresso come literal     *
      *                                                                *
      *                     - Pt = Points                              *
      *                     - In = Inches                              *
      *                     - Mm = Millimeters                         *
      *                     - Cm = Centimeters                         *
      *                                                                *
      *             oppure                                             *
      *                                                                *
      *          ps-num-scf = Scaling factor espresso come valore      *
      *                       numerico in numero di punti              *
      *                                                                *
      * Output : ps-lit-scf : rinormalizzato a spaces                  *
      *                                                                *
      *          ps-num-scf : rinormalizzato a zero                    *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = Rotate                                            *
      *                                                                *
      *          Rotazione del piano delle coordinate in senso anti-   *
      *          orario.                                               *
      *                                                                *
      * Input  : ps-rot-ang = Rotation angle, in gradi                 *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = Translate                                         *
      *                                                                *
      *          Traslazione del piano delle coordinate.               *
      *                                                                *
      * Input  : ps-hor-tra = Horizontal translation, espressa in base *
      *                       allo scaling factor attivo               *
      *                                                                *
      *          ps-ver-tra = Vertical translation, espressa in base   *
      *                       allo scaling factor attivo               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = Scale                                             *
      *                                                                *
      *          Scala del piano delle coordinate.                     *
      *                                                                *
      * Input  : ps-hor-sca = Horizontal scale                         *
      *                                                                *
      *          ps-ver-sca = Vertical scale                           *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetLineWidth                                      *
      *                                                                *
      *          Set dello spessore delle linee da tracciare.          *
      *                                                                *
      * Input  : ps-lin-wid = Line width, espresso in Pt               *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetDash                                           *
      *                                                                *
      *          Set della modalita' di tratteggio delle linee da      *
      *          tracciare.                                            *
      *                                                                *
      * Input  : ps-das-pat = Dash pattern, strettamente secondo la    *
      *                       sintassi PostScript, di cui seguono      *
      *                       alcuni esempi:                           *
      *                                                                *
      *                       [] 0 = Solid                             *
      *                       [3 3] 0 = 3 on 3 off ...                 *
      *                       [5 2 2 2] 0 = 5 on 2 off 2 on 2 off ...  *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetLineCap                                        *
      *                                                                *
      *          Set del parametro di controllo per la forma della     *
      *          parte finale delle linee in tracciati aperti.         *
      *                                                                *
      * Input  : ps-lin-cap = Line cap                                 *
      *                                                                *
      *                     - Butt                                     *
      *                     - Round                                    *
      *                     - ProjectingSquare                         *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetLineJoin                                       *
      *                                                                *
      *          Set del parametro di controllo per la forma della     *
      *          giunzione di linee consecutive.                       *
      *                                                                *
      * Input  : ps-lin-joi = Line join                                *
      *                                                                *
      *                     - Miter                                    *
      *                     - Round                                    *
      *                     - Bevel                                    *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetMiterLimit                                     *
      *                                                                *
      *          Set del parametro di limite per la forma di giunzione *
      *          linea Miter.                                          *
      *                                                                *
      * Input  : ps-mit-lim = Miter limit                              *
      *                                                                *
      *                     - 90degrees                                *
      *                     - 60degrees                                *
      *                     - 11degrees                                *
      *                     - Always                                   *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetStrokeRGBColor                                 *
      *                                                                *
      *          Set del colore per la tracciatura delle linee         *
      *          in termini di percentuali RGB.                        *
      *                                                                *
      * Input  : ps-rgb-red = Percentuale di rosso                     *
      *                                                                *
      *          ps-rgb-gre = Percentuale di verde                     *
      *                                                                *
      *          ps-rgb-blu = Percentuale di blu                       *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetStrokeHSBColor                                 *
      *                                                                *
      *          Set del colore per la tracciatura delle linee         *
      *          in termini di percentuali HSB.                        *
      *                                                                *
      * Input  : ps-hsb-hue = Percentuale di tinta                     *
      *                                                                *
      *          ps-hsb-sat = Percentuale di saturazione               *
      *                                                                *
      *          ps-hsb-bri = Percentuale di brillantezza              *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetStrokeCMYKColor                                *
      *                                                                *
      *          Set del colore per la tracciatura delle linee         *
      *          in termini di percentuali CMYK.                       *
      *                                                                *
      * Input  : ps-cmy-cya = Percentuale di ciano                     *
      *                                                                *
      *          ps-cmy-mag = Percentuale di magenta                   *
      *                                                                *
      *          ps-cmy-yel = Percentuale di giallo                    *
      *                                                                *
      *          ps-cmy-bla = Percentuale di nero                      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetStrokeGrayColor                                *
      *                                                                *
      *          Set del colore per la tracciatura delle linee         *
      *          in termini di grigio.                                 *
      *                                                                *
      * Input  : ps-gra-bla = Percentuale di nero                      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetStrokeColor                                    *
      *                                                                *
      *          Set del colore per la tracciatura delle linee         *
      *          in termini di colore predefinito.                     *
      *                                                                *
      * Input  : ps-col-nam = Color name                               *
      *                                                                *
      *                     - Black                                    *
      *                     - Red                                      *
      *                     - Green                                    *
      *                     - Blue                                     *
      *                     - Yellow                                   *
      *                     - Magenta                                  *
      *                     - Cyan                                     *
      *                     - White                                    *
      *                     - 5%Gray                                   *
      *                     - 10%Gray                                  *
      *                     - 15%Gray                                  *
      *                     - 20%Gray                                  *
      *                     - 25%Gray                                  *
      *                     - 30%Gray                                  *
      *                     - 35%Gray                                  *
      *                     - 40%Gray                                  *
      *                     - 45%Gray                                  *
      *                     - 50%Gray                                  *
      *                     - 55%Gray                                  *
      *                     - 60%Gray                                  *
      *                     - 65%Gray                                  *
      *                     - 70%Gray                                  *
      *                     - 75%Gray                                  *
      *                     - 80%Gray                                  *
      *                     - 85%Gray                                  *
      *                     - 90%Gray                                  *
      *                     - 95%Gray                                  *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetFillRGBColor                                   *
      *                                                                *
      *          Set del colore per il riempimento delle forme         *
      *          in termini di percentuali RGB.                        *
      *                                                                *
      * Input  : ps-rgb-red = Percentuale di rosso                     *
      *                                                                *
      *          ps-rgb-gre = Percentuale di verde                     *
      *                                                                *
      *          ps-rgb-blu = Percentuale di blu                       *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetFillHSBColor                                   *
      *                                                                *
      *          Set del colore per il riempimento delle forme         *
      *          in termini di percentuali HSB.                        *
      *                                                                *
      * Input  : ps-hsb-hue = Percentuale di tinta                     *
      *                                                                *
      *          ps-hsb-sat = Percentuale di saturazione               *
      *                                                                *
      *          ps-hsb-bri = Percentuale di brillantezza              *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetFillCMYKColor                                  *
      *                                                                *
      *          Set del colore per il riempimento delle forme         *
      *          in termini di percentuali CMYK.                       *
      *                                                                *
      * Input  : ps-cmy-cya = Percentuale di ciano                     *
      *                                                                *
      *          ps-cmy-mag = Percentuale di magenta                   *
      *                                                                *
      *          ps-cmy-yel = Percentuale di giallo                    *
      *                                                                *
      *          ps-cmy-bla = Percentuale di nero                      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetFillGrayColor                                  *
      *                                                                *
      *          Set del colore per il riempimento delle forme         *
      *          in termini di grigio.                                 *
      *                                                                *
      * Input  : ps-gra-bla = Percentuale di nero                      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetFillColor                                      *
      *                                                                *
      *          Set del colore per il riempimento delle forme         *
      *          in termini di colore predefinito.                     *
      *                                                                *
      * Input  : ps-col-nam = Color name                               *
      *                                                                *
      *                     - Black                                    *
      *                     - Red                                      *
      *                     - Green                                    *
      *                     - Blue                                     *
      *                     - Yellow                                   *
      *                     - Magenta                                  *
      *                     - Cyan                                     *
      *                     - White                                    *
      *                     - 5%Gray                                   *
      *                     - 10%Gray                                  *
      *                     - 15%Gray                                  *
      *                     - 20%Gray                                  *
      *                     - 25%Gray                                  *
      *                     - 30%Gray                                  *
      *                     - 35%Gray                                  *
      *                     - 40%Gray                                  *
      *                     - 45%Gray                                  *
      *                     - 50%Gray                                  *
      *                     - 55%Gray                                  *
      *                     - 60%Gray                                  *
      *                     - 65%Gray                                  *
      *                     - 70%Gray                                  *
      *                     - 75%Gray                                  *
      *                     - 80%Gray                                  *
      *                     - 85%Gray                                  *
      *                     - 90%Gray                                  *
      *                     - 95%Gray                                  *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetTextRGBColor                                   *
      *                                                                *
      *          Set del colore per la tracciatura del testo           *
      *          in termini di percentuali RGB.                        *
      *                                                                *
      * Input  : ps-rgb-red = Percentuale di rosso                     *
      *                                                                *
      *          ps-rgb-gre = Percentuale di verde                     *
      *                                                                *
      *          ps-rgb-blu = Percentuale di blu                       *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetTextHSBColor                                   *
      *                                                                *
      *          Set del colore per la tracciatura del testo           *
      *          in termini di percentuali HSB.                        *
      *                                                                *
      * Input  : ps-hsb-hue = Percentuale di tinta                     *
      *                                                                *
      *          ps-hsb-sat = Percentuale di saturazione               *
      *                                                                *
      *          ps-hsb-bri = Percentuale di brillantezza              *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetTextCMYKColor                                  *
      *                                                                *
      *          Set del colore per la tracciatura del testo           *
      *          in termini di percentuali CMYK.                       *
      *                                                                *
      * Input  : ps-cmy-cya = Percentuale di ciano                     *
      *                                                                *
      *          ps-cmy-mag = Percentuale di magenta                   *
      *                                                                *
      *          ps-cmy-yel = Percentuale di giallo                    *
      *                                                                *
      *          ps-cmy-bla = Percentuale di nero                      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetTextGrayColor                                  *
      *                                                                *
      *          Set del colore per la tracciatura del testo           *
      *          in termini di grigio.                                 *
      *                                                                *
      * Input  : ps-gra-bla = Percentuale di nero                      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetTextColor                                      *
      *                                                                *
      *          Set del colore per la tracciatura del testo           *
      *          in termini di colore predefinito.                     *
      *                                                                *
      * Input  : ps-col-nam = Color name                               *
      *                                                                *
      *                     - Black                                    *
      *                     - Red                                      *
      *                     - Green                                    *
      *                     - Blue                                     *
      *                     - Yellow                                   *
      *                     - Magenta                                  *
      *                     - Cyan                                     *
      *                     - White                                    *
      *                     - 5%Gray                                   *
      *                     - 10%Gray                                  *
      *                     - 15%Gray                                  *
      *                     - 20%Gray                                  *
      *                     - 25%Gray                                  *
      *                     - 30%Gray                                  *
      *                     - 35%Gray                                  *
      *                     - 40%Gray                                  *
      *                     - 45%Gray                                  *
      *                     - 50%Gray                                  *
      *                     - 55%Gray                                  *
      *                     - 60%Gray                                  *
      *                     - 65%Gray                                  *
      *                     - 70%Gray                                  *
      *                     - 75%Gray                                  *
      *                     - 80%Gray                                  *
      *                     - 85%Gray                                  *
      *                     - 90%Gray                                  *
      *                     - 95%Gray                                  *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetRoundingRadius                                 *
      *                                                                *
      *          Set del raggio per l'arrotondamento dei rettangoli    *
      *          arrotondati, in Pt.                                   *
      *                                                                *
      * Input  : ps-rou-rad = Rounding radius                          *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetShadingOffset                                  *
      *                                                                *
      *          Set dell'offset di spostamento sia in basso che a de- *
      *          stra, per i rettangoli ombreggiati, in Pt.            *
      *                                                                *
      * Input  : ps-sha-off = Shading offset                           *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SetShadingGrayColor                               *
      *                                                                *
      *          Set del colore per l'ombreggiatura dei rettangoli,    *
      *          in termini di grigio.                                 *
      *                                                                *
      * Input  : ps-sha-bla = Percentuale di nero                      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = SelectFont                                        *
      *                                                                *
      *          Set del font in un determinato size.                  *
      *                                                                *
      * Input  : ps-fon-nam = Font name                                *
      *                                                                *
      *                       - Times-Roman                            *
      *                       - Times-Italic                           *
      *                       - Times-Bold                             *
      *                       - Times-BoldItalic                       *
      *                       - Helvetica                              *
      *                       - Helvetica-Oblique                      *
      *                       - Helvetica-Bold                         *
      *                       - Helvetica-BoldOblique                  *
      *                       - Courier                                *
      *                       - Courier-Oblique                        *
      *                       - Courier-Bold                           *
      *                       - Courier-BoldOblique                    *
      *                       - Symbol                                 *
      *                                                                *
      *          ps-fon-siz = Font size                                *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = Line                                              *
      *                                                                *
      *          Tracciamento di una linea.                            *
      *                                                                *
      * Input  : ps-coo-xxx = Punto di origine, coordinata x           *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yyy = Punto di origine, coordinata y           *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-xto = Punto di destinazione, coordinata x      *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yto = Punto di destinazione, coordinata y      *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = RectStroke                                        *
      *                                                                *
      *          Tracciamento di un rettangolo, solamente i bordi,     *
      *          senza riempimento.                                    *
      *                                                                *
      * Input  : ps-coo-xxx = Punto in basso a sinistra, coordinata x  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yyy = Punto in basso a sinistra, coordinata y  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-xto = Punto in alto a destra, coordinata x     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yto = Punto in alto a destra, coordinata y     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = RectFill                                          *
      *                                                                *
      *          Tracciamento di un rettangolo, solamente il riempi-   *
      *          mento, senza i bordi.                                 *
      *                                                                *
      * Input  : ps-coo-xxx = Punto in basso a sinistra, coordinata x  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yyy = Punto in basso a sinistra, coordinata y  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-xto = Punto in alto a destra, coordinata x     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yto = Punto in alto a destra, coordinata y     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = Rect                                              *
      *                                                                *
      *          Tracciamento di un rettangolo, sia i bordi che il     *
      *          riempimento.                                          *
      *                                                                *
      * Input  : ps-coo-xxx = Punto in basso a sinistra, coordinata x  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yyy = Punto in basso a sinistra, coordinata y  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-xto = Punto in alto a destra, coordinata x     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yto = Punto in alto a destra, coordinata y     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = RoundRectStroke                                   *
      *                                                                *
      *          Tracciamento di un rettangolo con angoli arrotondati, *
      *          solamente i bordi, senza riempimento.                 *
      *                                                                *
      * Input  : ps-coo-xxx = Punto in basso a sinistra, coordinata x  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yyy = Punto in basso a sinistra, coordinata y  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-xto = Punto in alto a destra, coordinata x     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yto = Punto in alto a destra, coordinata y     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = RoundRectFill                                     *
      *                                                                *
      *          Tracciamento di un rettangolo con angoli arrotondati, *
      *          solamente il riempimento, senza i bordi.              *
      *                                                                *
      * Input  : ps-coo-xxx = Punto in basso a sinistra, coordinata x  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yyy = Punto in basso a sinistra, coordinata y  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-xto = Punto in alto a destra, coordinata x     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yto = Punto in alto a destra, coordinata y     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = RoundRect                                         *
      *                                                                *
      *          Tracciamento di un rettangolo con angoli arrotondati, *
      *          sia i bordi che il riempimento.                       *
      *                                                                *
      * Input  : ps-coo-xxx = Punto in basso a sinistra, coordinata x  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yyy = Punto in basso a sinistra, coordinata y  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-xto = Punto in alto a destra, coordinata x     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yto = Punto in alto a destra, coordinata y     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = ShadowRect                                        *
      *                                                                *
      *          Tracciamento di un rettangolo, sia i bordi che il     *
      *          riempimento, con ombreggiatura tridimensionale.       *
      *                                                                *
      * Input  : ps-coo-xxx = Punto in basso a sinistra, coordinata x  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yyy = Punto in basso a sinistra, coordinata y  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-xto = Punto in alto a destra, coordinata x     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yto = Punto in alto a destra, coordinata y     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = ShadowRoundRect                                   *
      *                                                                *
      *          Tracciamento di un rettangolo con angoli arrotondati, *
      *          sia i bordi che il riempimento, con ombreggiatura     *
      *          tridimensionale.                                      *
      *                                                                *
      * Input  : ps-coo-xxx = Punto in basso a sinistra, coordinata x  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yyy = Punto in basso a sinistra, coordinata y  *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-xto = Punto in alto a destra, coordinata x     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      *          ps-coo-yto = Punto in alto a destra, coordinata y     *
      *                       espressa in unita' di misura attiva      *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = BaseLPrint                                        *
      *                                                                *
      *          Tracciamento di un testo, con baseline alla coordi-   *
      *          nata y, allineato a sinistra alla coordinata x.       *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = BaseCPrint                                        *
      *                                                                *
      *          Tracciamento di un testo, con baseline alla coordi-   *
      *          nata y, allineato al centro alla coordinata x.        *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = BaseRPrint                                        *
      *                                                                *
      *          Tracciamento di un testo, con baseline alla coordi-   *
      *          nata y, allineato a destra alla coordinata x.         *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = LPrint                                            *
      *                                                                *
      *          Tracciamento di un testo, con appoggio alla coordi-   *
      *          nata y, allineato a sinistra alla coordinata x.       *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = CPrint                                            *
      *                                                                *
      *          Tracciamento di un testo, con appoggio alla coordi-   *
      *          nata y, allineato al centro alla coordinata x.        *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = RPrint                                            *
      *                                                                *
      *          Tracciamento di un testo, con appoggio alla coordi-   *
      *          nata y, allineato a destra alla coordinata x.         *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = BoxBaseLPrint                                     *
      *                                                                *
      *          Tracciamento di un testo all'interno di un box, con   *
      *          baseline sulla linea inferiore del rettangolo, alli-  *
      *          neato a sinistra al lato sinistro del rettangolo.     *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-xto = Coordinata x del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-coo-yto = Coordinata y del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = BoxBaseCPrint                                     *
      *                                                                *
      *          Tracciamento di un testo all'interno di un box, con   *
      *          baseline sulla linea inferiore del rettangolo, alli-  *
      *          neato al centro del rettangolo.                       *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-xto = Coordinata x del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-coo-yto = Coordinata y del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = BoxBaseRPrint                                     *
      *                                                                *
      *          Tracciamento di un testo all'interno di un box, con   *
      *          baseline sulla linea inferiore del rettangolo, alli-  *
      *          neato a destra al lato destro del rettangolo.         *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-xto = Coordinata x del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-coo-yto = Coordinata y del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = BoxLPrint                                         *
      *                                                                *
      *          Tracciamento di un testo all'interno di un box, cen-  *
      *          trato verticalmente all'interno del box, allineato    *
      *          orizzontalmente a sinistra al lato sinistro del ret-  *
      *          tangolo.                                              *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-xto = Coordinata x del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-coo-yto = Coordinata y del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = BoxCPrint                                         *
      *                                                                *
      *          Tracciamento di un testo all'interno di un box, cen-  *
      *          trato verticalmente all'interno del box, allineato    *
      *          orizzontalmente al centro del rettangolo.             *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-xto = Coordinata x del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-coo-yto = Coordinata y del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = BoxRPrint                                         *
      *                                                                *
      *          Tracciamento di un testo all'interno di un box, cen-  *
      *          trato verticalmente all'interno del box, allineato    *
      *          orizzontalmente a destra al lato destro del rettan-   *
      *          golo.                                                 *
      *                                                                *
      * Input  : ps-coo-xxx = Coordinata x del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-yyy = Coordinata y del punto in basso a sini-  *
      *                       stra del box, espressa in unita' di mi-  *
      *                       sura attiva                              *
      *                                                                *
      *          ps-coo-xto = Coordinata x del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-coo-yto = Coordinata y del punto in alto a destra  *
      *                       del box, espressa in unita' di misura    *
      *                       attiva                                   *
      *                                                                *
      *          ps-str-alf = Stringa alfanumerica da tracciare        *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = IncludeEPS                                        *
      *                                                                *
      *          Inclusione di un modulo in formato EPS.               *
      *                                                                *
      * Input  : ps-eps-pat = Pathname del file che contiene il modu-  *
      *                       lo EPS                                   *
      *                                                                *
      * Output : Nessuno                                               *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * ps-tip-ope = PostScript                                        *
      *                                                                *
      *          Esecuzione di un comando PostScript, la cui defini-   *
      *          zione puo' constare al massimo di 10 linee di 240     *
      *          caratteri ciascuna.                                   *
      *                                                                *
      * Input  : ps-pos-001 = Linea 01                                 *
      *          ps-pos-002 = Linea 02                                 *
      *          ps-pos-002 = Linea 03                                 *
      *          ps-pos-002 = Linea 04                                 *
      *          ps-pos-002 = Linea 05                                 *
      *          ps-pos-002 = Linea 06                                 *
      *          ps-pos-002 = Linea 07                                 *
      *          ps-pos-002 = Linea 08                                 *
      *          ps-pos-002 = Linea 09                                 *
      *          ps-pos-002 = Linea 10                                 *
      *                                                                *
      * Output : Dopo l'esecuzione dell'operazione tutte le linee, da  *
      *          01 a 10, vengono rinormalizzate.                      *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     Alter-Srl-PD .
       Object-Computer.     Alter-Srl-PD .

       Special-Names.       Decimal-Point is comma .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [pfc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pfc   assign to disk           f-pfc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pfc-k01
                   alternate record key   is pfc-k02
                   alternate record key   is pfc-k03
                   alternate record key   is pfc-k04
                   alternate record key   is pfc-k05
                             with   duplicates
                             file status  is                f-pfc-sts .
       
      *    *===========================================================*
      *    * File Control [pss]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pss   assign to disk       f-pss-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pss-key
                             file status  is            f-pss-sts     .

      *    *===========================================================*
      *    * File Control [stp]                                        *
      *    *-----------------------------------------------------------*
           select optional   stp   assign to input-output   f-stp-pat
                             organization is line sequential
                             access  mode is sequential
                             file status  is                f-stp-sts .

      *    *===========================================================*
      *    * File Control [st2]                                        *
      *    *-----------------------------------------------------------*
           select optional   st2   assign to input-output   f-st2-pat
                             organization is binary sequential
                             access  mode is sequential
                             file status  is                f-st2-sts .

      *    *===========================================================*
      *    * File Control [st3]                                        *
      *    *-----------------------------------------------------------*
           select optional   st3   assign to input-output   f-st3-pat
                             organization is binary sequential
                             access  mode is sequential
                             file status  is                f-st3-sts .

      *    *===========================================================*
      *    * File Control [eps]                                        *
      *    *-----------------------------------------------------------*
           select optional   eps   assign to input-output   f-eps-pat
                             organization is binary sequential
                             access  mode is sequential
                             file status  is                f-eps-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [pfc]                                    *
      *    *-----------------------------------------------------------*
       fd  pfc       label record standard                            .
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  pfc-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  pfc-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01                                  *
      *            *---------------------------------------------------*
               10  pfc-k01.
                   15  pfc-num-prg-001    pic  9(12)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02                                  *
      *            *---------------------------------------------------*
               10  pfc-k02.
                   15  pfc-cod-azi-002    pic  x(04)                  .
                   15  pfc-num-prg-002    pic  9(12)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03                                  *
      *            *---------------------------------------------------*
               10  pfc-k03.
                   15  pfc-cod-azi-003    pic  x(04)                  .
                   15  pfc-cod-ute-003    pic  x(08)                  .
                   15  pfc-num-prg-003    pic  9(12)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04                                  *
      *            *---------------------------------------------------*
               10  pfc-k04.
                   15  pfc-cod-ute-004    pic  x(08)                  .
                   15  pfc-num-prg-004    pic  9(12)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 05                                  *
      *            *---------------------------------------------------*
               10  pfc-k05.
                   15  pfc-cod-ute-005    pic  x(08)                  .
                   15  pfc-cod-azi-005    pic  x(04)                  .
                   15  pfc-num-prg-005    pic  9(12)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pfc-dat.
      *            *---------------------------------------------------*
      *            * Tipo di file catalogato                           *
      *            * - 01 : Print file                                 *
      *            * - 02 : Spool file                                 *
      *            *---------------------------------------------------*
               10  pfc-tip-fil            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero progressivo : 000000000001..000000999999   *
      *            *---------------------------------------------------*
               10  pfc-num-prg            pic  9(12)                  .
      *            *---------------------------------------------------*
      *            * Codice azienda relativa al file                   *
      *            *---------------------------------------------------*
               10  pfc-cod-azi            pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Codice terminale che ha creato il file            *
      *            *---------------------------------------------------*
               10  pfc-cod-ter            pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Codice utente che ha creato il file               *
      *            *---------------------------------------------------*
               10  pfc-cod-ute            pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Sistema applicativo del programma che ha creato   *
      *            * il file                                           *
      *            *---------------------------------------------------*
               10  pfc-ide-sap            pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Area gestionale del programma che ha creato il    *
      *            * file                                              *
      *            *---------------------------------------------------*
               10  pfc-ide-arg            pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Settore gestionale del programma che ha creato il *
      *            * file                                              *
      *            *---------------------------------------------------*
               10  pfc-ide-set            pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Fase gestionale del programma che ha creato il    *
      *            * file                                              *
      *            *---------------------------------------------------*
               10  pfc-ide-fas            pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Data di inizio creazione : s.aa.mm.gg             *
      *            *---------------------------------------------------*
               10  pfc-dat-icr            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ora di inizio creazione : hh.mm.ss.cc             *
      *            *---------------------------------------------------*
               10  pfc-ora-icr            pic  9(08)                  .
      *            *---------------------------------------------------*
      *            * Data di fine creazione : s.aa.mm.gg               *
      *            *---------------------------------------------------*
               10  pfc-dat-fcr            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ora di fine creazione : hh.mm.ss.cc               *
      *            *---------------------------------------------------*
               10  pfc-ora-fcr            pic  9(08)                  .
      *            *---------------------------------------------------*
      *            * Numero di pagine create                           *
      *            *---------------------------------------------------*
               10  pfc-nmr-pgn            pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Pathname del file di stampa                       *
      *            *---------------------------------------------------*
               10  pfc-pat-stp            pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice stampante selezionata in creazione         *
      *            *---------------------------------------------------*
               10  pfc-cod-stp            pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Codice modulo selezionata in creazione            *
      *            *---------------------------------------------------*
               10  pfc-cod-mod            pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Annotazioni 1                                     *
      *            *---------------------------------------------------*
               10  pfc-not-001            pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Annotazioni 2                                     *
      *            *---------------------------------------------------*
               10  pfc-not-002            pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Numero di stampe effettive eseguite e terminate   *
      *            *---------------------------------------------------*
               10  pfc-num-sef            pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Data inizio ultima stampa effettiva eseguita e    *
      *            * terminata                                         *
      *            *---------------------------------------------------*
               10  pfc-dat-ius            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ora di inizio ultima stampa effettiva eseguita e  *
      *            * terminata                                         *
      *            *---------------------------------------------------*
               10  pfc-ora-ius            pic  9(08)                  .
      *            *---------------------------------------------------*
      *            * Data fine ultima stampa effettiva eseguita e ter- *
      *            * minata                                            *
      *            *---------------------------------------------------*
               10  pfc-dat-fus            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ora di fine ultima stampa effettiva eseguita e    *
      *            * terminata                                         *
      *            *---------------------------------------------------*
               10  pfc-ora-fus            pic  9(08)                  .
      *            *---------------------------------------------------*
      *            * Data inizio stampa in esecuzione                  *
      *            *---------------------------------------------------*
               10  pfc-dat-ise            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ora di inizio stampa in esecuzione                *
      *            *---------------------------------------------------*
               10  pfc-ora-ise            pic  9(08)                  .
      *            *---------------------------------------------------*
      *            * Area libera per espansioni future                 *
      *            *---------------------------------------------------*
               10  pfc-alx-fut.
                   15  filler occurs 80   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [pss]                                    *
      *    *-----------------------------------------------------------*
       fd  pss       label record standard                            .
       01  pss-rec.
           05  pss-key.
               10  pss-tre                pic  x(04)                  .
               10  pss-kre                pic  x(40)                  .
           05  pss-dat.
               10  pss-chr.
                   15  filler      occurs 1536
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [stp]                                    *
      *    *-----------------------------------------------------------*
       fd  stp       label record omitted                             .
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  stp-rec.
           05  stp-chr occurs 240         pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [st2]                                    *
      *    *-----------------------------------------------------------*
       fd  st2  label record omitted.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  st2-rec.
      *        *-------------------------------------------------------*
      *        * Caratteri componenti il record, 1024 caratteri        *
      *        *-------------------------------------------------------*
           05  st2-chr occurs 1024        pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [st3]                                    *
      *    *-----------------------------------------------------------*
       fd  st3  label record omitted.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  st3-rec.
      *        *-------------------------------------------------------*
      *        * Caratteri componenti il record, 1 carattere           *
      *        *-------------------------------------------------------*
           05  st3-chr                    pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [eps]                                    *
      *    *-----------------------------------------------------------*
       fd  eps       label record omitted                             .
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  eps-rec.
      *        *-------------------------------------------------------*
      *        * Caratteri componenti il record, 1024 caratteri        *
      *        *-------------------------------------------------------*
           05  eps-chr occurs 1024        pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Work area per trattamento [pfc] riguardante il cobol      *
      *    *-----------------------------------------------------------*
       01  f-pfc.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-pfc-nam                  pic  x(04) value "pfc "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-pfc-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-pfc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Work area per trattamento [pss] riguardante il cobol      *
      *    *-----------------------------------------------------------*
       01  f-pss.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-pss-nam                  pic  x(04) value "pss "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-pss-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-pss-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Work area per trattamento [stp] riguardante il cobol      *
      *    *-----------------------------------------------------------*
       01  f-stp.
      *        *-------------------------------------------------------*
      *        * Cobol file name                                       *
      *        *-------------------------------------------------------*
           05  f-stp-nam                  pic  x(04) value "stp "     .
      *        *-------------------------------------------------------*
      *        * Cobol file pathname                                   *
      *        *-------------------------------------------------------*
           05  f-stp-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Cobol file status                                     *
      *        *-------------------------------------------------------*
           05  f-stp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Work area per trattamento [st2] riguardante il cobol      *
      *    *-----------------------------------------------------------*
       01  f-st2.
      *        *-------------------------------------------------------*
      *        * Cobol file name                                       *
      *        *-------------------------------------------------------*
           05  f-st2-nam                  pic  x(04) value "st2 "     .
      *        *-------------------------------------------------------*
      *        * Cobol file pathname                                   *
      *        *-------------------------------------------------------*
           05  f-st2-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Cobol file status                                     *
      *        *-------------------------------------------------------*
           05  f-st2-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Work area per trattamento [st3] riguardante il cobol      *
      *    *-----------------------------------------------------------*
       01  f-st3.
      *        *-------------------------------------------------------*
      *        * Cobol file name                                       *
      *        *-------------------------------------------------------*
           05  f-st3-nam                  pic  x(04) value "st3 "     .
      *        *-------------------------------------------------------*
      *        * Cobol file pathname                                   *
      *        *-------------------------------------------------------*
           05  f-st3-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Cobol file status                                     *
      *        *-------------------------------------------------------*
           05  f-st3-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [eps]                *
      *    *-----------------------------------------------------------*
       01  f-eps.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-eps-nam                  pic  x(04) value "eps "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-eps-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-eps-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Work per records di [pss]                                 *
      *    *-----------------------------------------------------------*
       01  w-pss.
      *        *-------------------------------------------------------*
      *        * Work per singolo file di configurazione tipo 'stp'    *
      *        *-------------------------------------------------------*
           05  w-pss-stp.
      *            *---------------------------------------------------*
      *            * Codice stampante                                  *
      *            *---------------------------------------------------*
               10  w-pss-stp-cod-stp      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Descrizione stampante                             *
      *            *---------------------------------------------------*
               10  w-pss-stp-des-stp      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Tipo stampante                                    *
      *            *---------------------------------------------------*
               10  w-pss-stp-tip-stp      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Canale di uscita                                  *
      *            *---------------------------------------------------*
               10  w-pss-stp-can-stp      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Password per la stampante                         *
      *            *---------------------------------------------------*
               10  w-pss-stp-pwd-stp      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Password per montaggio modulo sulla stampante     *
      *            *---------------------------------------------------*
               10  w-pss-stp-pwd-mms      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Flag di inibizione del form feed finale in caso   *
      *            * di stampa in spool                                *
      *            * - N : Non inibito, valore di default              *
      *            * - S : Inibito                                     *
      *            *---------------------------------------------------*
               10  w-pss-stp-ibz-fff      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per trasformazione in uppercase                 *
      *    *-----------------------------------------------------------*
       01  w-upp-cas.
      *        *-------------------------------------------------------*
      *        * Valore in lowercase da trasformare in uppercase       *
      *        *-------------------------------------------------------*
           05  w-upp-cas-str.
               10  w-upp-cas-chr
                              occurs 240  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work area locale                                      *
      *        *-------------------------------------------------------*
           05  w-upp-cas-c00              pic  9(03)                  .
           05  w-upp-cas-c01              pic  9(03)                  .
           05  w-upp-cas-c02              pic  9(03)                  .
           05  w-upp-cas-low              pic  x(26)       value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-upp-cas-upp              pic  x(26)       value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .

      *    *===========================================================*
      *    * Work-area per editing valori numerici                     *
      *    *-----------------------------------------------------------*
       01  w-edt-num.
      *        *-------------------------------------------------------*
      *        * Valore da editare                                     *
      *        *-------------------------------------------------------*
           05  w-edt-num-num              pic s9(05)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Valore editato                                        *
      *        *-------------------------------------------------------*
           05  w-edt-num-e01              pic  x(10)                  .
           05  w-edt-num-e02              pic  x(10)                  .
           05  w-edt-num-e03              pic  x(10)                  .
           05  w-edt-num-e04              pic  x(10)                  .
           05  w-edt-num-e05              pic  x(10)                  .
           05  w-edt-num-e06              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Work area locale                                      *
      *        *-------------------------------------------------------*
           05  w-edt-num-exx              pic  x(10)                  .
           05  w-edt-num-eyy              pic  x(10)                  .
           05  w-edt-num-r01 redefines
               w-edt-num-eyy.
               10  w-edt-num-chr
                                occurs 10 pic  x(01)                  .
           05  w-edt-num-r02 redefines
               w-edt-num-eyy.
               10  w-edt-num-ned          pic -z(04)9(01),9(03)       .
           05  w-edt-num-i01              pic  9(02)                  .
           05  w-edt-num-i02              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per terminazione stringa alfanumerica           *
      *    *-----------------------------------------------------------*
       01  w-ter-str.
      *        *-------------------------------------------------------*
      *        * Stringa da terminare con high value                   *
      *        *-------------------------------------------------------*
           05  w-ter-str-alf.
               10  w-ter-str-alx
                              occurs 180  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Stringa terminata con high value                      *
      *        *-------------------------------------------------------*
           05  w-ter-str-h01.
               10  w-ter-str-h1x
                              occurs 180  pic  x(01)                  .
           05  w-ter-str-h02.
               10  w-ter-str-h2x
                              occurs 180  pic  x(01)                  .
           05  w-ter-str-h03.
               10  w-ter-str-h3x
                              occurs 180  pic  x(01)                  .
           05  w-ter-str-h04.
               10  w-ter-str-h4x
                              occurs 180  pic  x(01)                  .
           05  w-ter-str-h05.
               10  w-ter-str-h5x
                              occurs 180  pic  x(01)                  .
           05  w-ter-str-h06.
               10  w-ter-str-h6x
                              occurs 180  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work area locale                                      *
      *        *-------------------------------------------------------*
           05  w-ter-str-hxx.
               10  w-ter-str-hyy
                              occurs 180  pic  x(01)                  .
           05  w-ter-str-i01              pic  9(03)                  .
           05  w-ter-str-i02              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per esecuzione job di stampa                    *
      *    *-----------------------------------------------------------*
       01  w-exe-job.
      *        *-------------------------------------------------------*
      *        * Flag di errore generale                               *
      *        *-------------------------------------------------------*
           05  w-exe-job-flg-err          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero di pagine generate dal job                     *
      *        *-------------------------------------------------------*
           05  w-exe-job-num-pag          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Page Width                                            *
      *        *-------------------------------------------------------*
           05  w-exe-job-pag-wid          pic  9(04)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Page Height                                           *
      *        *-------------------------------------------------------*
           05  w-exe-job-pag-hei          pic  9(04)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Page Orientation                                      *
      *        *-------------------------------------------------------*
           05  w-exe-job-pag-ori          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo per file di spool                  *
      *        *-------------------------------------------------------*
           05  w-exe-job-npr-fds          pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Pathname per file di spool                            *
      *        *-------------------------------------------------------*
           05  w-exe-job-pth-fds          pic  x(60)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di output                                        *
      *        *                                                       *
      *        * - S : Spooler di sistema                              *
      *        * - R : rcp                                             *
      *        * - L : Stampante locale via terminale                  *
      *        * - F : File                                            *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-exe-job-tip-out          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Canale di stampa letto da codice stampante [pss]      *
      *        *-------------------------------------------------------*
           05  w-exe-job-can-stp          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per suddivisione canale di stampa in due parti *
      *        *-------------------------------------------------------*
           05  w-exe-job-can-001          pic  x(40)                  .
           05  w-exe-job-can-002          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Shell script per il rilascio del file di spool        *
      *        *-------------------------------------------------------*
           05  w-exe-job-cmd-shs.
               15  filler occurs 220      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per '%%CreationDate:'                            *
      *        *-------------------------------------------------------*
           05  w-exe-job-crd.
               10  w-exe-job-crd-mmm      pic  9(02)                  .
               10  w-exe-job-crd-t01      pic  x(01) value "-"        .
               10  w-exe-job-crd-ddd      pic  9(02)                  .
               10  w-exe-job-crd-t02      pic  x(01) value "-"        .
               10  w-exe-job-crd-yyy      pic  9(04)                  .
               10  w-exe-job-crd-t03      pic  x(01) value spaces     .
               10  w-exe-job-crd-hhh      pic  9(02)                  .
               10  w-exe-job-crd-t04      pic  x(01) value ":"        .
               10  w-exe-job-crd-min      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per calcoli                                    *
      *        *-------------------------------------------------------*
           05  w-exe-job-cal-c01          pic s9(04)v9(03)            .
           05  w-exe-job-cal-c02          pic s9(04)v9(03)            .
           05  w-exe-job-cal-c03          pic s9(04)v9(03)            .
           05  w-exe-job-cal-c04          pic s9(04)v9(03)            .

      *    *===========================================================*
      *    * Work-area per la costruzione del comando di sistema per   *
      *    * la chiamata del system spooler                            *
      *    *-----------------------------------------------------------*
       01  w-sys-spl.
      *        *-------------------------------------------------------*
      *        * Default per il template dello spooler di stampa       *
      *        * per lo spooler di stampa, a 'lpr'                     *
      *        *-------------------------------------------------------*
           05  w-sys-spl-cmd-def          pic  x(60) value
                     "(p=@p; f=@f; cat $f | lpr -P $p -h -l -s; rm -f $f
      -              ") &       "                                     .
      *        *-------------------------------------------------------*
      *        * Area per la personalizzazione relativa al template    *
      *        * per lo spooler di stampa                              *
      *        *-------------------------------------------------------*
           05  w-sys-spl-cmd-tem.
               10  w-sys-spl-cmd-tex
                               occurs 60  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per composizione comando di sistema              *
      *        *-------------------------------------------------------*
           05  w-sys-spl-cmd.
      *            *---------------------------------------------------*
      *            * Codice della stampante in 'printcap'              *
      *            *---------------------------------------------------*
               10  w-sys-spl-cmd-cst      pic  x(13)                  .
      *            *---------------------------------------------------*
      *            * Pathname del file da stampare                     *
      *            *---------------------------------------------------*
               10  w-sys-spl-cmd-pat      pic  x(60)                  .
      *            *---------------------------------------------------*
      *            * Redirezione output ed errori                      *
      *            *---------------------------------------------------*
               10  w-sys-spl-cmd-roe      pic  x(30) value
                     " 1>/dev/null 2>&1             "                 .
      *        *-------------------------------------------------------*
      *        * Contatori, indici, puntatori, locali                  *
      *        *-------------------------------------------------------*
           05  w-sys-spl-inx-tem          pic  9(03)                  .
           05  w-sys-spl-pnt-tem          pic  9(03)                  .
           05  w-sys-spl-inx-hpr          pic  9(03)                  .
           05  w-sys-spl-ctr-001          pic  9(03)                  .
           05  w-sys-spl-ctr-002          pic  9(03)                  .
           05  w-sys-spl-wkx-060          pic  x(60)                  .

      *    *===========================================================*
      *    * Work-area per la costruzione del comando di sistema per   *
      *    * la chiamata via local printer                             *
      *    *-----------------------------------------------------------*
       01  w-loc-prn.
      *        *-------------------------------------------------------*
      *        * Pathname per file "locprn00"                          *
      *        *-------------------------------------------------------*
           05  w-loc-prn-prn-000          pic  x(60)                  .
      *        *-------------------------------------------------------*
      *        * Pathname per file "locprn99"                          *
      *        *-------------------------------------------------------*
           05  w-loc-prn-prn-999          pic  x(60)                  .

      *    *===========================================================*
      *    * Work per trattamento file [eps]                           *
      *    *-----------------------------------------------------------*
       01  w-trt-fil-eps.
      *        *-------------------------------------------------------*
      *        * Pathname del file [eps] da trattare                   *
      *        *-------------------------------------------------------*
           05  w-trt-fil-eps-pat          pic  x(60)                  .
      *        *-------------------------------------------------------*
      *        * Contatore trailing low-values del record letto        *
      *        *-------------------------------------------------------*
           05  w-trt-fil-eps-tlw          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore caratteri effettivi del record letto        *
      *        *-------------------------------------------------------*
           05  w-trt-fil-eps-eff          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Indice per scansione del record                       *
      *        *-------------------------------------------------------*
           05  w-trt-fil-eps-inx          pic  9(05)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mposts"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/ps"                                 .

      ******************************************************************
       Procedure Division                 using ps                    .
      ******************************************************************

      *================================================================*
      *      Declaratives                                              *
      *================================================================*
       Declaratives.
       Decl-pfc-sec Section.
           Use after standard error procedure on pfc.
       decl-pfc.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-pfc-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-pfc                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-pfc-sts            to   e-sts                  .
       Decl-pss-sec Section.
           Use after standard error procedure on pss.
       decl-pss.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-pss-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-pss                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-pss-sts            to   e-sts                  .
       Decl-stp-sec Section.
           Use after standard error procedure on stp.
       decl-stp.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-stp-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-stp                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-stp-sts            to   e-sts                  .
       Decl-st2-sec Section.
           Use after standard error procedure on st2.
       decl-st2.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-stp-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-st2                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-st2-sts            to   e-sts                  .
       Decl-st3-sec Section.
           Use after standard error procedure on st3.
       decl-st3.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-st3-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-st3                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-st3-sts            to   e-sts                  .
       End Declaratives.

      *================================================================*
      *    Main                                                        *
      *================================================================*
       Main Section.
       main-000.
      *              *-------------------------------------------------*
      *              * Trasformazione del tipo operazione in uppercase *
      *              *-------------------------------------------------*
           move      ps-tip-ope           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-tip-ope             .
      *              *-------------------------------------------------*
      *              * Esecuzione subroutine in funzione del tipo ope- *
      *              * razione                                         *
      *              *-------------------------------------------------*
           if        ps-tip-ope           =
                            "JOBINIT                                 "
                     perform job-ini-000  thru job-ini-999
           else if   ps-tip-ope           =
                            "JOBSTART                                "
                     perform job-str-000  thru job-str-999
           else if   ps-tip-ope           =
                            "JOBEND                                  "
                     perform job-end-000  thru job-end-999
           else if   ps-tip-ope           =
                            "JOBPROLOGSTART                          "
                     perform job-prs-000  thru job-prs-999
           else if   ps-tip-ope           =
                            "JOBPROLOGEND                            "
                     perform job-pre-000  thru job-pre-999
           else if   ps-tip-ope           =
                            "JOBSETUPSTART                           "
                     perform job-sus-000  thru job-sus-999
           else if   ps-tip-ope           =
                            "JOBSETUPEND                             "
                     perform job-sue-000  thru job-sue-999
           else if   ps-tip-ope           =
                            "PAGESTART                               "
                     perform pag-str-000  thru pag-str-999
           else if   ps-tip-ope           =
                            "PAGEEND                                 "
                     perform pag-end-000  thru pag-end-999
           else if   ps-tip-ope           =
                            "SAVEGLOBAL                              "
                     perform sav-glo-000  thru sav-glo-999
           else if   ps-tip-ope           =
                            "RESTOREGLOBAL                           "
                     perform res-glo-000  thru res-glo-999
           else if   ps-tip-ope           =
                            "SAVEGRAPHICSSTATE                       "
                     perform sav-grs-000  thru sav-grs-999
           else if   ps-tip-ope           =
                            "RESTOREGRAPHICSSTATE                    "
                     perform res-grs-000  thru res-grs-999
           else if   ps-tip-ope           =
                            "SETHORIZONTALFACTOR                     "
                     perform set-hfa-000  thru set-hfa-999
           else if   ps-tip-ope           =
                            "SETVERTICALFACTOR                       "
                     perform set-vfa-000  thru set-vfa-999
           else if   ps-tip-ope           =
                            "SETHORIZONTALVERTICALFACTOR             "
                     perform set-hvf-000  thru set-hvf-999
           else if   ps-tip-ope           =
                            "ROTATE                                  "
                     perform rot-ate-000  thru rot-ate-999
           else if   ps-tip-ope           =
                            "TRANSLATE                               "
                     perform tra-nsl-000  thru tra-nsl-999
           else if   ps-tip-ope           =
                            "SCALE                                   "
                     perform sca-lex-000  thru sca-lex-999
           else if   ps-tip-ope           =
                            "SETLINEWIDTH                            "
                     perform lin-wid-000  thru lin-wid-999
           else if   ps-tip-ope           =
                            "SETDASH                                 "
                     perform das-hxx-000  thru das-hxx-999
           else if   ps-tip-ope           =
                            "SETLINECAP                              "
                     perform lin-cap-000  thru lin-cap-999
           else if   ps-tip-ope           =
                            "SETLINEJOIN                             "
                     perform lin-joi-000  thru lin-joi-999
           else if   ps-tip-ope           =
                            "SETMITERLIMIT                           "
                     perform mit-lim-000  thru mit-lim-999
           else if   ps-tip-ope           =
                            "SETSTROKERGBCOLOR                       "
                     perform str-rgb-000  thru str-rgb-999
           else if   ps-tip-ope           =
                            "SETSTROKEHSBCOLOR                       "
                     perform str-hsb-000  thru str-hsb-999
           else if   ps-tip-ope           =
                            "SETSTROKECMYKCOLOR                      "
                     perform str-cmy-000  thru str-cmy-999
           else if   ps-tip-ope           =
                            "SETSTROKEGRAYCOLOR                      "
                     perform str-gra-000  thru str-gra-999
           else if   ps-tip-ope           =
                            "SETSTROKECOLOR                          "
                     perform str-col-000  thru str-col-999
           else if   ps-tip-ope           =
                            "SETFILLRGBCOLOR                         "
                     perform fil-rgb-000  thru fil-rgb-999
           else if   ps-tip-ope           =
                            "SETFILLHSBCOLOR                         "
                     perform fil-hsb-000  thru fil-hsb-999
           else if   ps-tip-ope           =
                            "SETFILLCMYKCOLOR                        "
                     perform fil-cmy-000  thru fil-cmy-999
           else if   ps-tip-ope           =
                            "SETFILLGRAYCOLOR                        "
                     perform fil-gra-000  thru fil-gra-999
           else if   ps-tip-ope           =
                            "SETFILLCOLOR                            "
                     perform fil-col-000  thru fil-col-999
           else if   ps-tip-ope           =
                            "SETTEXTRGBCOLOR                         "
                     perform txt-rgb-000  thru txt-rgb-999
           else if   ps-tip-ope           =
                            "SETTEXTHSBCOLOR                         "
                     perform txt-hsb-000  thru txt-hsb-999
           else if   ps-tip-ope           =
                            "SETTEXTCMYKCOLOR                        "
                     perform txt-cmy-000  thru txt-cmy-999
           else if   ps-tip-ope           =
                            "SETTEXTGRAYCOLOR                        "
                     perform txt-gra-000  thru txt-gra-999
           else if   ps-tip-ope           =
                            "SETTEXTCOLOR                            "
                     perform txt-col-000  thru txt-col-999
           else if   ps-tip-ope           =
                            "SETROUNDINGRADIUS                       "
                     perform rou-rad-000  thru rou-rad-999
           else if   ps-tip-ope           =
                            "SETSHADINGOFFSET                        "
                     perform sha-off-000  thru sha-off-999
           else if   ps-tip-ope           =
                            "SETSHADINGGRAYCOLOR                     "
                     perform sha-gra-000  thru sha-gra-999
           else if   ps-tip-ope           =
                            "SELECTFONT                              "
                     perform sel-fon-000  thru sel-fon-999
           else if   ps-tip-ope           =
                            "LINE                                    "
                     perform lin-exx-000  thru lin-exx-999
           else if   ps-tip-ope           =
                            "RECTSTROKE                              "
                     perform rec-str-000  thru rec-str-999
           else if   ps-tip-ope           =
                            "RECTFILL                                "
                     perform rec-fil-000  thru rec-fil-999
           else if   ps-tip-ope           =
                            "RECT                                    "
                     perform rec-txx-000  thru rec-txx-999
           else if   ps-tip-ope           =
                            "ROUNDRECTSTROKE                         "
                     perform rrc-str-000  thru rrc-str-999
           else if   ps-tip-ope           =
                            "ROUNDRECTFILL                           "
                     perform rrc-fil-000  thru rrc-fil-999
           else if   ps-tip-ope           =
                            "ROUNDRECT                               "
                     perform rrc-txx-000  thru rrc-txx-999
           else if   ps-tip-ope           =
                            "SHADOWRECT                              "
                     perform sha-rec-000  thru sha-rec-999
           else if   ps-tip-ope           =
                            "SHADOWROUNDRECT                         "
                     perform sha-rrc-000  thru sha-rrc-999
           else if   ps-tip-ope           =
                            "BASELPRINT                              "
                     perform bas-lpr-000  thru bas-lpr-999
           else if   ps-tip-ope           =
                            "BASECPRINT                              "
                     perform bas-cpr-000  thru bas-cpr-999
           else if   ps-tip-ope           =
                            "BASERPRINT                              "
                     perform bas-rpr-000  thru bas-rpr-999
           else if   ps-tip-ope           =
                            "LPRINT                                  "
                     perform lpr-int-000  thru lpr-int-999
           else if   ps-tip-ope           =
                            "CPRINT                                  "
                     perform cpr-int-000  thru cpr-int-999
           else if   ps-tip-ope           =
                            "RPRINT                                  "
                     perform rpr-int-000  thru rpr-int-999
           else if   ps-tip-ope           =
                            "BOXBASELPRINT                           "
                     perform bxb-lpr-000  thru bxb-lpr-999
           else if   ps-tip-ope           =
                            "BOXBASECPRINT                           "
                     perform bxb-cpr-000  thru bxb-cpr-999
           else if   ps-tip-ope           =
                            "BOXBASERPRINT                           "
                     perform bxb-rpr-000  thru bxb-rpr-999
           else if   ps-tip-ope           =
                            "BOXLPRINT                               "
                     perform box-lpr-000  thru box-lpr-999
           else if   ps-tip-ope           =
                            "BOXCPRINT                               "
                     perform box-cpr-000  thru box-cpr-999
           else if   ps-tip-ope           =
                            "BOXRPRINT                               "
                     perform box-rpr-000  thru box-rpr-999
           else if   ps-tip-ope           =
                            "INCLUDEEPS                              "
                     perform inc-eps-000  thru inc-eps-999
           else if   ps-tip-ope           =
                            "POSTSCRIPT                              "
                     perform pos-scr-000  thru pos-scr-999            .
       main-999.
           exit      program                                          .

      *================================================================*
      *    JobInit                                                     *
      *----------------------------------------------------------------*
       job-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione di tutte le variabili dell'area *
      *              * di comunicazione, escluso ps-tip-ope            *
      *              *-------------------------------------------------*
           move      spaces               to   ps-cod-stp             .
           move      spaces               to   ps-pag-siz             .
           move      zero                 to   ps-pag-wid             .
           move      zero                 to   ps-pag-hei             .
           move      spaces               to   ps-pag-ori             .
           move      spaces               to   ps-lit-scf             .
           move      zero                 to   ps-num-scf             .
           move      zero                 to   ps-rot-ang             .
           move      zero                 to   ps-hor-tra             .
           move      zero                 to   ps-ver-tra             .
           move      zero                 to   ps-hor-sca             .
           move      zero                 to   ps-ver-sca             .
           move      zero                 to   ps-lin-wid             .
           move      spaces               to   ps-das-pat             .
           move      spaces               to   ps-lin-cap             .
           move      spaces               to   ps-lin-joi             .
           move      spaces               to   ps-mit-lim             .
           move      zero                 to   ps-rgb-red             .
           move      zero                 to   ps-rgb-gre             .
           move      zero                 to   ps-rgb-blu             .
           move      zero                 to   ps-hsb-hue             .
           move      zero                 to   ps-hsb-sat             .
           move      zero                 to   ps-hsb-bri             .
           move      zero                 to   ps-cmy-cya             .
           move      zero                 to   ps-cmy-mag             .
           move      zero                 to   ps-cmy-yel             .
           move      zero                 to   ps-cmy-bla             .
           move      zero                 to   ps-gra-bla             .
           move      spaces               to   ps-col-nam             .
           move      zero                 to   ps-rou-rad             .
           move      zero                 to   ps-sha-off             .
           move      zero                 to   ps-sha-bla             .
           move      spaces               to   ps-fon-nam             .
           move      zero                 to   ps-fon-siz             .
           move      zero                 to   ps-coo-xxx             .
           move      zero                 to   ps-coo-yyy             .
           move      zero                 to   ps-coo-xto             .
           move      zero                 to   ps-coo-yto             .
           move      spaces               to   ps-str-alf             .
           move      spaces               to   ps-eps-pat             .
           move      spaces               to   ps-pos-001             .
           move      spaces               to   ps-pos-002             .
           move      spaces               to   ps-pos-003             .
           move      spaces               to   ps-pos-004             .
           move      spaces               to   ps-pos-005             .
           move      spaces               to   ps-pos-006             .
           move      spaces               to   ps-pos-007             .
           move      spaces               to   ps-pos-008             .
           move      spaces               to   ps-pos-009             .
           move      spaces               to   ps-pos-010             .
           move      spaces               to   ps-par-alf             .
           move      spaces               to   ps-are-exp             .
       job-ini-999.
           exit.

      *================================================================*
      *    JobStart                                                    *
      *----------------------------------------------------------------*
       job-str-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           perform   job-str-nor-ini-000  thru job-str-nor-ini-999    .
      *              *-------------------------------------------------*
      *              * Determinazione page width ed height             *
      *              *-------------------------------------------------*
           perform   job-str-det-pwh-000  thru job-str-det-pwh-999    .
      *              *-------------------------------------------------*
      *              * Determinazione page orientation                 *
      *              *-------------------------------------------------*
           perform   job-str-det-por-000  thru job-str-det-por-999    .
      *              *-------------------------------------------------*
      *              * Determinazione numero progressivo file di spool *
      *              *-------------------------------------------------*
           perform   job-str-det-nfs-000  thru job-str-det-nfs-999    .
      *              *-------------------------------------------------*
      *              * Determinazione canale di stampa                 *
      *              *-------------------------------------------------*
           perform   job-str-det-cds-000  thru job-str-det-cds-999    .
      *              *-------------------------------------------------*
      *              * Apertura file [stp] in output                   *
      *              *-------------------------------------------------*
           perform   opn-out-fil-stp-000  thru opn-out-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Scrittura 'Commments'                           *
      *              *-------------------------------------------------*
           perform   job-str-wrt-com-000  thru job-str-wrt-com-999    .
       job-str-999.
           exit.

      *================================================================*
      *    JobStart - Normalizzazioni iniziali                         *
      *----------------------------------------------------------------*
       job-str-nor-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di errore generale di e-   *
      *              * secuzione                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-job-flg-err      .
      *              *-------------------------------------------------*
      *              * Normalizzazione numero di pagine generate       *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-job-num-pag      .
       job-str-nor-ini-999.
           exit.

      *================================================================*
      *    JobStart - Determinazione page width ed height              *
      *----------------------------------------------------------------*
       job-str-det-pwh-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del page size       *
      *              *-------------------------------------------------*
           move      ps-pag-siz           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-pag-siz             .
       job-str-det-pwh-100.
      *              *-------------------------------------------------*
      *              * Determinazione page width e page height in ter- *
      *              * mini di Pt                                      *
      *              *-------------------------------------------------*
           if        ps-pag-siz           =
                           "CUSTOM                                  "
                     move  ps-pag-wid     to   w-exe-job-pag-wid
                     move  ps-pag-hei     to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "SLIDE                                   "
                     move  527,760        to   w-exe-job-pag-wid
                     move  792,000        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "LETTER                                  "
                     move  612,000        to   w-exe-job-pag-wid
                     move  792,000        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "LEGAL                                   "
                     move  612,000        to   w-exe-job-pag-wid
                     move  1008,000       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "TABLOID                                 "
                     move  792,000        to   w-exe-job-pag-wid
                     move  1224,000       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "STATEMENT-HALF                          "
                     move  396,000        to   w-exe-job-pag-wid
                     move  612,000        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "EXECUTIVE                               "
                     move  522,000        to   w-exe-job-pag-wid
                     move  756,000        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "FANFOLD                                 "
                     move  792,000        to   w-exe-job-pag-wid
                     move  1071,360       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "DOUBLE                                  "
                     move  792,000        to   w-exe-job-pag-wid
                     move  1224,000       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "BROAD-SHEET                             "
                     move  1296,000       to   w-exe-job-pag-wid
                     move  1728,000       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "A0                                      "
                     move  2383,937       to   w-exe-job-pag-wid
                     move  3370,394       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "A1                                      "
                     move  1683,780       to   w-exe-job-pag-wid
                     move  2383,937       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "A2                                      "
                     move  1190,551       to   w-exe-job-pag-wid
                     move  1683,780       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "A3                                      "
                     move  841,890        to   w-exe-job-pag-wid
                     move  1190,551       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "A4                                      "
                     move  595,276        to   w-exe-job-pag-wid
                     move  841,890        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "A5                                      "
                     move  419,528        to   w-exe-job-pag-wid
                     move  595,276        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "A6                                      "
                     move  297,638        to   w-exe-job-pag-wid
                     move  419,528        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "B1-ISO                                  "
                     move  2004,094       to   w-exe-job-pag-wid
                     move  2834,646       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "B4-ISO                                  "
                     move  708,661        to   w-exe-job-pag-wid
                     move  1000,630       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "B5-ISO                                  "
                     move  498,898        to   w-exe-job-pag-wid
                     move  708,661        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "B4-JIS                                  "
                     move  728,504        to   w-exe-job-pag-wid
                     move  1031,811       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "B5-JIS                                  "
                     move  515,906        to   w-exe-job-pag-wid
                     move  728,504        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "C3                                      "
                     move  918,425        to   w-exe-job-pag-wid
                     move  1298,268       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "C4                                      "
                     move  649,134        to   w-exe-job-pag-wid
                     move  918,425        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "C5                                      "
                     move  459,213        to   w-exe-job-pag-wid
                     move  649,134        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "C6                                      "
                     move  323,150        to   w-exe-job-pag-wid
                     move  459,213        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "RA2                                     "
                     move  1218,898       to   w-exe-job-pag-wid
                     move  1729,134       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "RA3                                     "
                     move  864,567        to   w-exe-job-pag-wid
                     move  1218,898       to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "RA4                                     "
                     move  609,449        to   w-exe-job-pag-wid
                     move  864,567        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "ENVELOPE-#9                             "
                     move  639,360        to   w-exe-job-pag-wid
                     move  279,360        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "ENVELOPE-#10                            "
                     move  684,000        to   w-exe-job-pag-wid
                     move  297,360        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "ENVELOPE-#11                            "
                     move  747,360        to   w-exe-job-pag-wid
                     move  324,000        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "ENVELOPE-#12                            "
                     move  792,000        to   w-exe-job-pag-wid
                     move  342,000        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "ENVELOPE-#14                            "
                     move  828,000        to   w-exe-job-pag-wid
                     move  360,000        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "ENVELOPE-MONARCH                        "
                     move  540,000        to   w-exe-job-pag-wid
                     move  279,360        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "ENVELOPE-CHECK                          "
                     move  617,760        to   w-exe-job-pag-wid
                     move  279,360        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "DL                                      "
                     move  623,622        to   w-exe-job-pag-wid
                     move  311,811        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "GERMAN-FANFOLD                          "
                     move  612,000        to   w-exe-job-pag-wid
                     move  864,000        to   w-exe-job-pag-hei
           else if   ps-pag-siz           =
                           "GERMAN-LEGAL-FANFOLD                    "
                     move  612,000        to   w-exe-job-pag-wid
                     move  936,000        to   w-exe-job-pag-hei
           else      move  "A0                                      "
                                          to   ps-pag-siz
                     go to job-str-det-pwh-100.
       job-str-det-pwh-200.
      *              *-------------------------------------------------*
      *              * Page width e page height in ps-pag-wid ed in    *
      *              * ps-pag-hei                                      *
      *              *-------------------------------------------------*
           move      w-exe-job-pag-wid    to   ps-pag-wid             .
           move      w-exe-job-pag-hei    to   ps-pag-hei             .
       job-str-det-pwh-999.
           exit.

      *================================================================*
      *    JobStart - Determinazione page orientation                  *
      *----------------------------------------------------------------*
       job-str-det-por-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del page orienta-   *
      *              * tion                                            *
      *              *-------------------------------------------------*
           move      ps-pag-ori           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-pag-ori             .
       job-str-det-por-100.
      *              *-------------------------------------------------*
      *              * Determinazione segnale di page orientation      *
      *              *-------------------------------------------------*
           if        ps-pag-ori           =
                           "PORTRAIT                                "
                     move  "P"            to   w-exe-job-pag-ori
           else if   ps-pag-ori           =
                           "LANDSCAPE                               "
                     move  "L"            to   w-exe-job-pag-ori
           else      move  "PORTRAIT                                "
                                          to   ps-pag-ori
                     go to job-str-det-por-100.
       job-str-det-por-999.
           exit.

      *================================================================*
      *    JobStart - Determinazione numero progressivo file di spool  *
      *----------------------------------------------------------------*
       job-str-det-nfs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-job-npr-fds      .
       job-str-det-nfs-100.
      *              *-------------------------------------------------*
      *              * Open input file [pfc]                           *
      *              *-------------------------------------------------*
       job-str-det-nfs-110.
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [pfc]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fpx"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "pfc"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-pfc-pat              .
      *                  *---------------------------------------------*
      *                  * Normalizzazione error-code                  *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           open      i-o    pfc                                       .
      *                  *---------------------------------------------*
      *                  * Se errore di Open, si continua come se il   *
      *                  * numero progressivo determinato fosse pari   *
      *                  * a zero                                      *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to job-str-det-nfs-600.
       job-str-det-nfs-200.
      *              *-------------------------------------------------*
      *              * Lettura da [pfc] del record relativo al numero  *
      *              * progressivo file di spool                       *
      *              *-------------------------------------------------*
       job-str-det-nfs-210.
      *                  *---------------------------------------------*
      *                  * Lettura record                              *
      *                  *---------------------------------------------*
           move      zero                 to   pfc-num-prg-001        .
           move      e-not-err            to   e-sts                  .
           read      pfc    invalid key
                            go to   job-str-det-nfs-220.
           go to     job-str-det-nfs-300.
       job-str-det-nfs-220.
      *                  *---------------------------------------------*
      *                  * Se record non esistente lo si costruisce    *
      *                  * al valore iniziale e poi si ricicla a ri-   *
      *                  * leggerlo                                    *
      *                  *---------------------------------------------*
       job-str-det-nfs-230.
      *                      *-----------------------------------------*
      *                      * Composizione record normalizzato        *
      *                      *-----------------------------------------*
           move      spaces               to   pfc-rec                .
           move      zero                 to   pfc-num-prg-001        .
           move      spaces               to   pfc-cod-azi-002        .
           move      zero                 to   pfc-num-prg-002        .
           move      spaces               to   pfc-cod-azi-003        .
           move      spaces               to   pfc-cod-ute-003        .
           move      zero                 to   pfc-num-prg-003        .
           move      spaces               to   pfc-cod-ute-004        .
           move      zero                 to   pfc-num-prg-004        .
           move      spaces               to   pfc-cod-ute-005        .
           move      spaces               to   pfc-cod-azi-005        .
           move      zero                 to   pfc-num-prg-005        .
           move      zero                 to   pfc-tip-fil            .
           move      zero                 to   pfc-num-prg            .
           move      spaces               to   pfc-cod-azi            .
           move      spaces               to   pfc-cod-ter            .
           move      spaces               to   pfc-cod-ute            .
           move      spaces               to   pfc-ide-sap            .
           move      spaces               to   pfc-ide-arg            .
           move      spaces               to   pfc-ide-set            .
           move      spaces               to   pfc-ide-fas            .
           move      zero                 to   pfc-dat-icr            .
           move      zero                 to   pfc-ora-icr            .
           move      zero                 to   pfc-dat-fcr            .
           move      zero                 to   pfc-ora-fcr            .
           move      zero                 to   pfc-nmr-pgn            .
           move      spaces               to   pfc-pat-stp            .
           move      spaces               to   pfc-cod-stp            .
           move      spaces               to   pfc-cod-mod            .
           move      spaces               to   pfc-not-001            .
           move      spaces               to   pfc-not-002            .
           move      zero                 to   pfc-num-sef            .
           move      zero                 to   pfc-dat-ius            .
           move      zero                 to   pfc-ora-ius            .
           move      zero                 to   pfc-dat-fus            .
           move      zero                 to   pfc-ora-fus            .
           move      zero                 to   pfc-dat-ise            .
           move      zero                 to   pfc-ora-ise            .
           move      spaces               to   pfc-alx-fut            .
      *                      *-----------------------------------------*
      *                      * Scrittura record normalizzato           *
      *                      *                                         *
      *                      * Se errore di scrittura, si continua co- *
      *                      * me se il numero progressivo determinato *
      *                      * fosse pari a zero                       *
      *                      *-----------------------------------------*
           move      e-not-err            to   e-sts                  .
           write     pfc-rec invalid key
                             go to   job-str-det-nfs-500.
           if        e-sts                not  = e-not-err
                     go to job-str-det-nfs-500.
      *                      *-----------------------------------------*
      *                      * Se scrittura Ok si ricicla a rileggere  *
      *                      * il record normalizzato                  *
      *                      *-----------------------------------------*
           go to     job-str-det-nfs-200.
       job-str-det-nfs-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento numero progressivo                *
      *              *-------------------------------------------------*
           if        pfc-num-prg          =    000000999999
                     move  zero           to   pfc-num-prg            .
           add       1                    to   pfc-num-prg            .
      *              *-------------------------------------------------*
      *              * Memorizzazione numero progressivo               *
      *              *-------------------------------------------------*
           move      pfc-num-prg          to   w-exe-job-npr-fds      .
       job-str-det-nfs-400.
      *              *-------------------------------------------------*
      *              * Riscrittura record aggiornato                   *
      *              *                                                 *
      *              * Se errore di scrittura, si continua come se il  *
      *              * numero progressivo determinato fosse pari a ze- *
      *              * ro                                              *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           rewrite   pfc-rec invalid key
                             move    zero to   w-exe-job-npr-fds
                             go to   job-str-det-nfs-500.
           if        e-sts                not  = e-not-err
                     move  zero           to   w-exe-job-npr-fds
                     go to job-str-det-nfs-500.
       job-str-det-nfs-500.
      *              *-------------------------------------------------*
      *              * Close file [pfc]                                *
      *              *-------------------------------------------------*
           close     pfc                                              .
       job-str-det-nfs-600.
      *              *-------------------------------------------------*
      *              * Costruzione del pathname per il file di spool   *
      *              *-------------------------------------------------*
           move      ".S"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    "sf"       delimited by   size
                     w-exe-job-npr-fds
                                delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-exe-job-pth-fds      .
       job-str-det-nfs-999.
           exit.

      *================================================================*
      *    JobStart - Determinazione canale di stampa                  *
      *----------------------------------------------------------------*
       job-str-det-cds-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-job-tip-out      .
           move      spaces               to   w-exe-job-can-stp      .
           move      spaces               to   w-exe-job-can-001      .
           move      spaces               to   w-exe-job-can-002      .
       job-str-det-cds-050.
      *              *-------------------------------------------------*
      *              * Open input file [pss]                           *
      *              *-------------------------------------------------*
       job-str-det-cds-060.
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [pss]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fpx"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "pss"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-pss-pat              .
      *                  *---------------------------------------------*
      *                  * Normalizzazione error-code                  *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           open      i-o    pss                                       .
      *                  *---------------------------------------------*
      *                  * Se errore di Open, si continua come se il   *
      *                  * canale di stampa determinato fosse a spa-   *
      *                  * ces                                         *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to job-str-det-cds-200.
       job-str-det-cds-100.
      *              *-------------------------------------------------*
      *              * Lettura da [pss] del record relativo al codice  *
      *              * stampante                                       *
      *              *-------------------------------------------------*
       job-str-det-cds-110.
      *                  *---------------------------------------------*
      *                  * Lettura record                              *
      *                  *                                             *
      *                  * Se non trovato : continuazione come se il   *
      *                  * canale di stampa determinato fosse a spaces *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           move      "stp"                to   pss-tre                .
           move      ps-cod-stp           to   pss-kre                .
           read      pss    with no lock
                            invalid key
                            go to   job-str-det-cds-150.
      *                  *---------------------------------------------*
      *                  * Se errori : come per record non trovato     *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to job-str-det-cds-150.
      *                  *---------------------------------------------*
      *                  * Memorizzazione canale di stampa del codice  *
      *                  * stampante                                   *
      *                  *---------------------------------------------*
           move      pss-dat              to   w-pss-stp              .
           move      w-pss-stp-can-stp    to   w-exe-job-can-stp      .
       job-str-det-cds-150.
      *              *-------------------------------------------------*
      *              * Close file [pss]                                *
      *              *-------------------------------------------------*
           close     pss                                              .
       job-str-det-cds-200.
      *              *-------------------------------------------------*
      *              * Se il canale di stampa e' a spaces oppure al    *
      *              * valore "spool" si forza il codice stesso della  *
      *              * stampante e si continua                         *
      *              *-------------------------------------------------*
           if        w-exe-job-can-stp    =    spaces  or
                     w-exe-job-can-stp    =    "spool"
                     move  "spool"        to   w-exe-job-can-001
                     move  ps-cod-stp     to   w-exe-job-can-002
                     go to job-str-det-cds-300.
       job-str-det-cds-250.
      *              *-------------------------------------------------*
      *              * Suddivisione del canale di stampa in due parti  *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-job-can-001      .
           move      spaces               to   w-exe-job-can-002      .
           unstring  w-exe-job-can-stp
                                delimited by   all spaces
                                          into w-exe-job-can-001
                                               w-exe-job-can-002      .
       job-str-det-cds-300.
      *              *-------------------------------------------------*
      *              * Determinazione del tipo di output a seconda del *
      *              * valore delle due parti suddivise, e deviazione  *
      *              * a seconda del tipo di output determinato        *
      *              *-------------------------------------------------*
           if        w-exe-job-can-001    =    "spool"
                     move  "S"            to   w-exe-job-tip-out
                     go to job-str-det-cds-350
           else if   w-exe-job-can-001    =    "rcp"
                     move  "R"            to   w-exe-job-tip-out
                     go to job-str-det-cds-400
           else if   w-exe-job-can-001    =    "#"
                     move  "L"            to   w-exe-job-tip-out
                     go to job-str-det-cds-450
           else      move  "F"            to   w-exe-job-tip-out
                     go to job-str-det-cds-500.
       job-str-det-cds-350.
      *              *-------------------------------------------------*
      *              * Se tipo output : S                              *
      *              *-------------------------------------------------*
       job-str-det-cds-360.
      *                  *---------------------------------------------*
      *                  * Costruzione del comando per il rilascio     *
      *                  * finale del file di stampa allo spooler di   *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "tem-spl"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-sys-spl-cmd-tem      .
           if        w-sys-spl-cmd-tem    =    spaces
                     move  w-sys-spl-cmd-def
                                          to   w-sys-spl-cmd-tem      .
           move      w-exe-job-can-002    to   w-sys-spl-cmd-cst      .
           move      w-exe-job-pth-fds    to   w-sys-spl-cmd-pat      .
           perform   def-cll-sys-spc-000  thru def-cll-sys-spc-999    .
           move      o-shs                to   w-exe-job-cmd-shs      .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     job-str-det-cds-600.
       job-str-det-cds-400.
      *              *-------------------------------------------------*
      *              * Se tipo output : R                              *
      *              *-------------------------------------------------*
       job-str-det-cds-410.
      *                  *---------------------------------------------*
      *                  * Costruzione del comando per il rilascio     *
      *                  * finale del file di stampa via "rcp"         *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-job-cmd-shs      .
           string    "rcp "
                                delimited by   size
                     w-exe-job-pth-fds
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-exe-job-can-002
                                delimited by   spaces
                     " ; rm -f "
                                delimited by   size
                     w-exe-job-pth-fds
                                delimited by   spaces
                     " &"
                                delimited by   size
                                          into w-exe-job-cmd-shs      .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     job-str-det-cds-600.
       job-str-det-cds-450.
      *              *-------------------------------------------------*
      *              * Se tipo output : L                              *
      *              *-------------------------------------------------*
       job-str-det-cds-460.
      *                  *---------------------------------------------*
      *                  * Costruzione del comando per il rilascio     *
      *                  * finale del file di stampa via local printer *
      *                  *---------------------------------------------*
       job-str-det-cds-462.
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per 'locprn00'    *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "run"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "locprn00"           to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-loc-prn-prn-000      .
       job-str-det-cds-464.
      *                      *-----------------------------------------*
      *                      * Preparazione pathname per 'locprn99'    *
      *                      *-----------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "run"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "locprn99"           to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-loc-prn-prn-999      .
       job-str-det-cds-466.
      *                      *-----------------------------------------*
      *                      * Costruzione del comando                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-exe-job-cmd-shs      .
           string    "cat "
                                delimited by   size
                     w-loc-prn-prn-000
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-exe-job-pth-fds
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-loc-prn-prn-999
                                delimited by   spaces
                     " ; "
                                delimited by   size
                     "rm -f "
                                delimited by   size
                     w-exe-job-pth-fds
                                delimited by   spaces
                                          into w-exe-job-cmd-shs      .
       job-str-det-cds-470.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     job-str-det-cds-600.
       job-str-det-cds-500.
      *              *-------------------------------------------------*
      *              * Se tipo output : F                              *
      *              *-------------------------------------------------*
       job-str-det-cds-510.
      *                  *---------------------------------------------*
      *                  * Comando per il rilascio finale del file di  *
      *                  * stampa a spaces                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-job-cmd-shs      .
           string    "cp "
                                delimited by   size
                     w-exe-job-pth-fds
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-exe-job-can-001
                                delimited by   spaces
                     " ; rm -f "
                                delimited by   size
                     w-exe-job-pth-fds
                                delimited by   spaces
                     " &"
                                delimited by   size
                                          into w-exe-job-cmd-shs      .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     job-str-det-cds-600.
       job-str-det-cds-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     job-str-det-cds-999.
       job-str-det-cds-999.
           exit.

      *================================================================*
      *    JobStart - Scrittura 'Commments'                            *
      *----------------------------------------------------------------*
       job-str-wrt-com-000.
      *              *-------------------------------------------------*
      *              * Linea : %!PS-Adobe-3.0                          *
      *              *-------------------------------------------------*
           move      "%!PS-Adobe-3.0"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-str-wrt-com-050.
      *              *-------------------------------------------------*
      *              * Linea : %%BoundingBox:                          *
      *              *-------------------------------------------------*
       job-str-wrt-com-051.
      *                  *---------------------------------------------*
      *                  * Editing page width                          *
      *                  *---------------------------------------------*
           move      w-exe-job-pag-wid    to   w-edt-num-num          .
           divide    1000                 into w-edt-num-num          .
           multiply  1000                 by   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *                  *---------------------------------------------*
      *                  * Editing page height                         *
      *                  *---------------------------------------------*
           move      w-exe-job-pag-hei    to   w-edt-num-num          .
           divide    1000                 into w-edt-num-num          .
           multiply  1000                 by   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
       job-str-wrt-com-052.
      *                  *---------------------------------------------*
      *                  * Test su page orientation                    *
      *                  *---------------------------------------------*
           if        w-exe-job-pag-ori    =    "L"
                     go to job-str-wrt-com-054.
       job-str-wrt-com-053.
      *                  *---------------------------------------------*
      *                  * Page orientation Portrait                   *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%BoundingBox: 0 0 "
                                delimited by   size
                     w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
           go to     job-str-wrt-com-100.
       job-str-wrt-com-054.
      *                  *---------------------------------------------*
      *                  * Page orientation Landscape                  *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%BoundingBox: 0 0 "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e01
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-str-wrt-com-100.
      *              *-------------------------------------------------*
      *              * Linea : %%Copyright:                            *
      *              *-------------------------------------------------*
           move      "%%Copyright: Sandro Carturan +393471558507"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%Creator:                              *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           move      spaces               to   stp-rec                .
           string    "%%Creator: tangram - "
                                delimited by   size
                     s-fas
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%CreationDate:                         *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           move      s-mes                to   w-exe-job-crd-mmm      .
           move      s-gio                to   w-exe-job-crd-ddd      .
           move      s-saa                to   w-exe-job-crd-yyy      .
           add       1900                 to   w-exe-job-crd-yyy      .
           move      s-ora                to   w-exe-job-crd-hhh      .
           move      s-min                to   w-exe-job-crd-min      .
      *
           move      spaces               to   stp-rec                .
           string    "%%CreationDate: "
                                delimited by   size
                     w-exe-job-crd
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%DocumentData:                         *
      *              *-------------------------------------------------*
           move      "%%DocumentData: Binary"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%For:                                  *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           move      spaces               to   stp-rec                .
           string    "%%For: "
                                delimited by   size
                     s-ute
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%LanguageLevel:                        *
      *              *-------------------------------------------------*
           move      "%%LanguageLevel: 2"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%Orientation:                          *
      *              *-------------------------------------------------*
           if        w-exe-job-pag-ori    =    "L"
                     move "%%Orientation: Landscape"
                                          to   stp-rec
           else      move "%%Orientation: Portrait"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%Pages:                                *
      *              *-------------------------------------------------*
           move      "%%Pages: (atend)"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%PageOrder:                            *
      *              *-------------------------------------------------*
           move      "%%PageOrder: Ascend"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%Title:                                *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           move      spaces               to   stp-rec                .
           string    "%%Title: "
                                delimited by   size
                     s-sap
                                delimited by   spaces
                     "-"        delimited by   size
                     s-arg
                                delimited by   spaces
                     "-"        delimited by   size
                     s-set
                                delimited by   spaces
                     "-"        delimited by   size
                     s-fas
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%Version:                              *
      *              *-------------------------------------------------*
           move      "%%Version: 1.1 1"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%EndComments                           *
      *              *-------------------------------------------------*
           move      "%%EndComments"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%BeginDefaults                         *
      *              *-------------------------------------------------*
           move      "%%BeginDefaults"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%EndDefaults                           *
      *              *-------------------------------------------------*
           move      "%%EndDefaults"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-str-wrt-com-999.
           exit.

      *================================================================*
      *    JobEnd                                                      *
      *----------------------------------------------------------------*
       job-end-000.
      *              *-------------------------------------------------*
      *              * Linea : '%%Trailer'                             *
      *              *-------------------------------------------------*
           move      "%%Trailer"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-end-100.
      *              *-------------------------------------------------*
      *              * Linea : '%%Pages'                               *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Editing numero pagine                      *
      *                   *--------------------------------------------*
           move      w-exe-job-num-pag    to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *                   *--------------------------------------------*
      *                   * Linea                                      *
      *                   *--------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%Pages: "
                                delimited by   size
                     w-edt-num-e01
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-end-200.
      *              *-------------------------------------------------*
      *              * Linea : '%%JobEnd'                              *
      *              *-------------------------------------------------*
           move      "JobEnd"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-end-300.
      *              *-------------------------------------------------*
      *              * Linea : '%%EOF'                                 *
      *              *-------------------------------------------------*
           move      "%%EOF"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-end-400.
      *              *-------------------------------------------------*
      *              * Linea : EndOfTransmission                       *
      *              *-------------------------------------------------*
           move      x'04'
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-end-500.
      *              *-------------------------------------------------*
      *              * Close file [stp] in output                      *
      *              *-------------------------------------------------*
           perform   cls-out-fil-stp-000  thru cls-out-fil-stp-999    .
       job-end-600.
      *              *-------------------------------------------------*
      *              * Rilascio del file [stp] allo spooler            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se comando a spaces : nessuna azione        *
      *                  *---------------------------------------------*
           if        w-exe-job-cmd-shs    =    spaces
                     go to job-end-900.
      *                  *---------------------------------------------*
      *                  * Chiamata al sistema effettiva               *
      *                  *---------------------------------------------*
           move      "CU"                 to   o-ope                  .
           move      w-exe-job-cmd-shs    to   o-shs                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       job-end-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     job-end-999.
       job-end-999.
           exit.

      *================================================================*
      *    JobPrologStart                                              *
      *----------------------------------------------------------------*
       job-prs-000.
      *              *-------------------------------------------------*
      *              * Linea : %%BeginProlog                           *
      *              *-------------------------------------------------*
           move      "%%BeginProlog"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Scrittura 'Prolog'                              *
      *              *-------------------------------------------------*
           perform   job-prs-wrt-pro-000  thru job-prs-wrt-pro-999    .
       job-prs-999.
           exit.

      *================================================================*
      *    JobPrologStart - Scrittura 'Prolog'                         *
      *================================================================*
       job-prs-wrt-pro-000.
           move      "/JobStart {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    save"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    initgraphics"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    (Pt) SetHorizontalVerticalFactor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    0.50 SetLineWidth"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    [] 0 SetDash"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    (Butt) SetLineCap"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    (Miter) SetLineJoin"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    (11Degrees) SetMiterLimit"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    0.0 SetStrokeGrayColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    0.0 SetFillGrayColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    0.0 SetTextGrayColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    2.0 2.82857 mul SetRoundingRadius"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    1.5 2.82857 mul SetShadingOffset"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    0.50 SetShadingGrayColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    (Courier) 10 SelectFont"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/JobEnd {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    restore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/ShowPage {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    showpage"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SaveGlobal {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    save"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/RestoreGlobal {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    restore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SaveGraphicsState {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/RestoreGraphicsState {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetHorizontalFactor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup type (stringtype) eq"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    { dup (Pt) eq { /HF 1       def } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "      dup (In) eq { /HF 72      def } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "      dup (Mm) eq { /HF 2.82857 def } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "      dup (Cm) eq { /HF 28.2857 def } if }"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "      dup /VF exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    }"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    ifelse"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    pop"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetVerticalFactor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup type (stringtype) eq"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    { dup (Pt) eq { /VF 1       def } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "      dup (In) eq { /VF 72      def } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "      dup (Mm) eq { /VF 2.82857 def } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "      dup (Cm) eq { /VF 28.2857 def } if }"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "      dup /VF exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    }"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    ifelse"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    pop"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetHorizontalVerticalFactor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    SetHorizontalFactor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    SetVerticalFactor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/Rotate {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    rotate"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/Translate {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    VU exch HU exch translate"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/Scale {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    scale"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetLineWidth {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setlinewidth"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetDash {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setdash"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetLineCap {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Butt)             eq { 0 setlinecap } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Round)            eq { 1 setlinecap } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (ProjectingSquare) eq { 2 setlinecap } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    pop"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetLineJoin {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Miter) eq { 0 setlinejoin } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Round) eq { 1 setlinejoin } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Bevel) eq { 2 setlinejoin } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    pop"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetMiterLimit {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (90Degrees) eq { 1.414 setmiterlimit } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (60Degrees) eq { 2.0   setmiterlimit } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (11Degrees) eq { 10.0  setmiterlimit } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Always)    eq { 1.0   setmiterlimit } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    pop"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetStrokeRGBColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetStrokeHSBColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    sethsbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetStrokeCMYKColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setcmykcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetStrokeGrayColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setgray"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetStrokeColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetNamedColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetFillRGBColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetFillHSBColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    sethsbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetFillCMYKColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setcmykcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetFillGrayColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setgray"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetFillColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetNamedColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetTextRGBColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetTextHSBColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    sethsbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetTextCMYKColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setcmykcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetTextGrayColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    setgray"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetTextColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetNamedColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    currentrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TBLU exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TGRN exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TRED exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetRoundingRadius {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /ROUR exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetShadingOffset {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SHAO exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SetShadingGrayColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /SHAB exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/SelectFont {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup /FONS exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    exch dup /FONT exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    cvn exch selectfont"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FONY currentfont /FontMatrix get 0 get"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "          currentfont /FontBBox get 1 get mul neg"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "          def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /FONH currentfont /FontMatrix get 0 get"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "          currentfont /FontBBox get 3 get mul FONY
      -              " add"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "          def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/Line {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    newpath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    closepath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetStrokeColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    stroke"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/RectStroke {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetStrokeColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    stroke"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/RectFill {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetFillColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    fill"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/Rect {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetFillColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    fill"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetStrokeColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    stroke"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/RoundRectStroke {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRoundRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetStrokeColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    stroke"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/RoundRectFill {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRoundRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetFillColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    fill"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/RoundRect {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRoundRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetFillColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    fill"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRoundRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetStrokeColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    stroke"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/ShadowRect {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    SHAO SHAO neg translate"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    SHAB setgray"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    fill"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 Y1 X2 Y2 Rect"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/ShadowRoundRect {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    SHAO SHAO neg translate"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XRoundRectPath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    SHAB setgray"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    fill"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 Y1 X2 Y2 RoundRect"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/BaseLPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/BaseCPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX stringwidth neg 2 div exch neg 2 div exch r
      -              "moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/BaseRPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX stringwidth neg exch neg exch rmoveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/LPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU FONY add moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/CPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU FONY add moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX stringwidth neg 2 div exch neg 2 div exch r
      -              "moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/RPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU FONY add moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX stringwidth neg exch neg exch rmoveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/BoxBaseLPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    newpath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto X1 HU Y2 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU lineto X2 HU Y1 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    closepath clip"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU FONY add moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/BoxBaseCPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    newpath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto X1 HU Y2 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU lineto X2 HU Y1 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    closepath clip"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU X1 HU add 2 div Y1 VU FONY add moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX stringwidth 2 div neg exch 2 div neg exch r
      -              "moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/BoxBaseRPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    newpath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto X1 HU Y2 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU lineto X2 HU Y1 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    closepath clip"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y1 VU FONY add moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX stringwidth neg exch neg exch rmoveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/BoxLPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    newpath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto X1 HU Y2 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU lineto X2 HU Y1 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    closepath clip"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y2 VU Y1 VU sub FONH sub 2 div FONY add 
      -              "Y1 VU add moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/BoxCPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    newpath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto X1 HU Y2 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU lineto X2 HU Y1 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    closepath clip"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU X1 HU add 2 div Y2 VU Y1 VU sub FONH sub
      -              " 2 div FONY add Y1 VU add moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX stringwidth 2 div neg exch 2 div neg exch r
      -              "moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/BoxRPrint {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /TX exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X2 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /Y1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    /X1 exch def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    newpath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto X1 HU Y2 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU lineto X2 HU Y1 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    closepath clip"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU Y1 VU sub FONH sub 2 div FONY add 
      -              "Y1 VU add moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX stringwidth neg exch neg exch rmoveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    XSetTextColor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TX show"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/HU {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    HF mul"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/VU {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    VF mul"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/XSetNamedColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Black)   eq { 0.00 0.00 0.00 setrgbcolor}
      -              " if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Red)     eq { 1.00 0.00 0.00 setrgbcolor}
      -              " if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Green)   eq { 0.00 1.00 0.00 setrgbcolor}
      -              " if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Blue)    eq { 0.00 0.00 1.00 setrgbcolor}
      -              " if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Yellow)  eq { 1.00 1.00 0.00 setrgbcolor}
      -              " if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Magenta) eq { 1.00 0.00 1.00 setrgbcolor}
      -              " if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (Cyan)    eq { 0.00 1.00 1.00 setrgbcolor}
      -              " if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (White)   eq { 1.00 1.00 1.00 setrgbcolor}
      -              " if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (5%Gray)  eq { 0.95 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (10%Gray) eq { 0.90 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (15%Gray) eq { 0.85 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (20%Gray) eq { 0.80 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (25%Gray) eq { 0.75 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (30%Gray) eq { 0.70 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (35%Gray) eq { 0.65 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (40%Gray) eq { 0.60 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (45%Gray) eq { 0.55 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (50%Gray) eq { 0.50 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (55%Gray) eq { 0.45 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (60%Gray) eq { 0.40 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (65%Gray) eq { 0.35 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (70%Gray) eq { 0.30 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (75%Gray) eq { 0.25 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (80%Gray) eq { 0.20 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (85%Gray) eq { 0.15 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (90%Gray) eq { 0.10 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    dup (95%Gray) eq { 0.05 setgray } if"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    pop"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/XSetStrokeColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    SRED SGRN SBLU setrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/XSetFillColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    FRED FGRN FBLU setrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/XSetTextColor {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    TRED TGRN TBLU setrgbcolor"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/XRectPath {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    newpath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y2 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y1 VU lineto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    closepath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "/XRoundRectPath {"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    newpath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU ROUR add Y1 VU moveto"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y1 VU X1 HU Y1 VU ROUR add ROUR arct"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X1 HU Y2 VU X1 HU ROUR add Y2 VU ROUR arct"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y2 VU X2 HU Y2 VU ROUR sub ROUR arct"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    X2 HU Y1 VU X2 HU ROUR sub Y1 VU ROUR arct"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    closepath"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
           move      "    } bind def"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-000    .
      *
       job-prs-wrt-pro-999.
           exit.

      *================================================================*
      *    JobPrologEnd                                                *
      *----------------------------------------------------------------*
       job-pre-000.
      *              *-------------------------------------------------*
      *              * Linea : %%EndProlog                             *
      *              *-------------------------------------------------*
           move      "%%EndProlog"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-pre-999.
           exit.

      *================================================================*
      *    JobSetupStart                                               *
      *----------------------------------------------------------------*
       job-sus-000.
      *              *-------------------------------------------------*
      *              * Linea : %%BeginSetup                            *
      *              *-------------------------------------------------*
           move      "%%BeginSetup"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : JobStart                                *
      *              *-------------------------------------------------*
           move      "JobStart"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-sus-999.
           exit.

      *================================================================*
      *    JobSetupEnd                                                 *
      *----------------------------------------------------------------*
       job-sue-000.
      *              *-------------------------------------------------*
      *              * Linea : %%EndSetup                              *
      *              *-------------------------------------------------*
           move      "%%EndSetup"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       job-sue-999.
           exit.

      *================================================================*
      *    PageStart                                                   *
      *----------------------------------------------------------------*
       pag-str-000.
      *              *-------------------------------------------------*
      *              * Incremento del numero pagina                    *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-job-num-pag      .
      *              *-------------------------------------------------*
      *              * Editing del numero pagina                       *
      *              *-------------------------------------------------*
           move      w-exe-job-num-pag    to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%Page:                                 *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "%%Page: "
                                delimited by   size
                     w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e01
                                delimited by   spaces
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%BeginPageSetup                        *
      *              *-------------------------------------------------*
           move      "%%BeginPageSetup"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pag-str-100.
      *              *-------------------------------------------------*
      *              * Rotazione e traslazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se orientamento Landscape              *
      *                  *---------------------------------------------*
           if        w-exe-job-pag-ori    not  = "L"
                     go to pag-str-200.
      *                  *---------------------------------------------*
      *                  * Rotazione di 90 gradi                       *
      *                  *---------------------------------------------*
           move      "90 rotate"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *                  *---------------------------------------------*
      *                  * Traslazione per allineare il punto 0 0      *
      *                  *---------------------------------------------*
           move      w-exe-job-pag-wid    to   w-edt-num-num          .
           multiply  -1                   by   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
           move      spaces               to   stp-rec                .
           string    "0 "
                                delimited by   size
                     w-edt-num-e01
                                delimited by   spaces
                     " translate"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pag-str-200.
      *              *-------------------------------------------------*
      *              * Linea : %%EndPageSetup                          *
      *              *-------------------------------------------------*
           move      "%%EndPageSetup"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pag-str-999.
           exit.

      *================================================================*
      *    PageEnd                                                     *
      *----------------------------------------------------------------*
       pag-end-000.
      *              *-------------------------------------------------*
      *              * Linea : ShowPage                                *
      *              *-------------------------------------------------*
           move      "ShowPage"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pag-end-999.
           exit.

      *================================================================*
      *    SaveGlobal                                                  *
      *----------------------------------------------------------------*
       sav-glo-000.
      *              *-------------------------------------------------*
      *              * Linea : SaveGlobal                              *
      *              *-------------------------------------------------*
           move      "SaveGlobal"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       sav-glo-999.
           exit.

      *================================================================*
      *    RestoreGlobal                                               *
      *----------------------------------------------------------------*
       res-glo-000.
      *              *-------------------------------------------------*
      *              * Linea : RestoreGlobal                           *
      *              *-------------------------------------------------*
           move      "RestoreGlobal"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       res-glo-999.
           exit.

      *================================================================*
      *    SaveGraphicsState                                           *
      *----------------------------------------------------------------*
       sav-grs-000.
      *              *-------------------------------------------------*
      *              * Linea : SaveGraphicsState                       *
      *              *-------------------------------------------------*
           move      "SaveGraphicsState"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       sav-grs-999.
           exit.

      *================================================================*
      *    RestoreGraphicsState                                        *
      *----------------------------------------------------------------*
       res-grs-000.
      *              *-------------------------------------------------*
      *              * Linea : RestoreGraphicsState                    *
      *              *-------------------------------------------------*
           move      "RestoreGraphicsState"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       res-grs-999.
           exit.

      *================================================================*
      *    SetHorizontalFactor                                         *
      *----------------------------------------------------------------*
       set-hfa-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase dello scaling fac-  *
      *              * tor espresso come literal                       *
      *              *-------------------------------------------------*
           move      ps-lit-scf           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-lit-scf             .
       set-hfa-100.
      *              *-------------------------------------------------*
      *              * Riconoscimento dello scaling factor espresso    *
      *              * come literal, e sua eventuale trasformazione    *
      *              *-------------------------------------------------*
           if        ps-lit-scf           =    "PT"
                     move  "Pt"           to   ps-lit-scf
                     go to set-hfa-200
           else if   ps-lit-scf           =    "IN"
                     move  "In"           to   ps-lit-scf
                     go to set-hfa-200
           else if   ps-lit-scf           =    "MM"
                     move  "Mm"           to   ps-lit-scf
                     go to set-hfa-200
           else if   ps-lit-scf           =    "CM"
                     move  "Cm"           to   ps-lit-scf
                     go to set-hfa-200
           else      go to set-hfa-300.
       set-hfa-200.
      *              *-------------------------------------------------*
      *              * Se scaling factor espresso come literal         *
      *              *-------------------------------------------------*
       set-hfa-210.
      *                  *---------------------------------------------*
      *                  * Linea : SetHorizontalFactor                 *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     ps-lit-scf
                                delimited by   spaces
                     ") SetHorizontalFactor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       set-hfa-220.
      *                  *---------------------------------------------*
      *                  * A rinormalizzazioni finali                  *
      *                  *---------------------------------------------*
           go to     set-hfa-900.
       set-hfa-300.
      *              *-------------------------------------------------*
      *              * Se scaling factor espresso come valore numerico *
      *              * in Pt                                           *
      *              *-------------------------------------------------*
       set-hfa-310.
      *                  *---------------------------------------------*
      *                  * Se scaling factor a zero lo si forza a 1    *
      *                  *---------------------------------------------*
           if        ps-num-scf           =    zero
                     move  1              to   ps-num-scf             .
       set-hfa-320.
      *                  *---------------------------------------------*
      *                  * Linea : SetHorizontalFactor                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing scaling factor numerico         *
      *                      *-----------------------------------------*
           move      ps-num-scf           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *                      *-----------------------------------------*
      *                      * Linea                                   *
      *                      *-----------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " SetHorizontalFactor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       set-hfa-900.
      *              *-------------------------------------------------*
      *              * Rinormalizzazioni finali                        *
      *              *-------------------------------------------------*
           move      spaces               to   ps-lit-scf             .
           move      spaces               to   ps-num-scf             .
       set-hfa-999.
           exit.

      *================================================================*
      *    SetVerticalFactor                                           *
      *----------------------------------------------------------------*
       set-vfa-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase dello scaling fac-  *
      *              * tor espresso come literal                       *
      *              *-------------------------------------------------*
           move      ps-lit-scf           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-lit-scf             .
       set-vfa-100.
      *              *-------------------------------------------------*
      *              * Riconoscimento dello scaling factor espresso    *
      *              * come literal, e sua eventuale trasformazione    *
      *              *-------------------------------------------------*
           if        ps-lit-scf           =    "PT"
                     move  "Pt"           to   ps-lit-scf
                     go to set-vfa-200
           else if   ps-lit-scf           =    "IN"
                     move  "In"           to   ps-lit-scf
                     go to set-vfa-200
           else if   ps-lit-scf           =    "MM"
                     move  "Mm"           to   ps-lit-scf
                     go to set-vfa-200
           else if   ps-lit-scf           =    "CM"
                     move  "Cm"           to   ps-lit-scf
                     go to set-vfa-200
           else      go to set-vfa-300.
       set-vfa-200.
      *              *-------------------------------------------------*
      *              * Se scaling factor espresso come literal         *
      *              *-------------------------------------------------*
       set-vfa-210.
      *                  *---------------------------------------------*
      *                  * Linea : SetVerticalFactor                   *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     ps-lit-scf
                                delimited by   spaces
                     ") SetVerticalFactor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       set-vfa-220.
      *                  *---------------------------------------------*
      *                  * A rinormalizzazioni finali                  *
      *                  *---------------------------------------------*
           go to     set-vfa-900.
       set-vfa-300.
      *              *-------------------------------------------------*
      *              * Se scaling factor espresso come valore numerico *
      *              * in Pt                                           *
      *              *-------------------------------------------------*
       set-vfa-310.
      *                  *---------------------------------------------*
      *                  * Se scaling factor a zero lo si forza a 1    *
      *                  *---------------------------------------------*
           if        ps-num-scf           =    zero
                     move  1              to   ps-num-scf             .
       set-vfa-320.
      *                  *---------------------------------------------*
      *                  * Linea : SetVerticalFactor                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing scaling factor numerico         *
      *                      *-----------------------------------------*
           move      ps-num-scf           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *                      *-----------------------------------------*
      *                      * Linea                                   *
      *                      *-----------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " SetVerticalFactor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       set-vfa-900.
      *              *-------------------------------------------------*
      *              * Rinormalizzazioni finali                        *
      *              *-------------------------------------------------*
           move      spaces               to   ps-lit-scf             .
           move      spaces               to   ps-num-scf             .
       set-vfa-999.
           exit.

      *================================================================*
      *    SetHorizontalVerticalFactor                                 *
      *----------------------------------------------------------------*
       set-hvf-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase dello scaling fac-  *
      *              * tor espresso come literal                       *
      *              *-------------------------------------------------*
           move      ps-lit-scf           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-lit-scf             .
       set-hvf-100.
      *              *-------------------------------------------------*
      *              * Riconoscimento dello scaling factor espresso    *
      *              * come literal, e sua eventuale trasformazione    *
      *              *-------------------------------------------------*
           if        ps-lit-scf           =    "PT"
                     move  "Pt"           to   ps-lit-scf
                     go to set-hvf-200
           else if   ps-lit-scf           =    "IN"
                     move  "In"           to   ps-lit-scf
                     go to set-hvf-200
           else if   ps-lit-scf           =    "MM"
                     move  "Mm"           to   ps-lit-scf
                     go to set-hvf-200
           else if   ps-lit-scf           =    "CM"
                     move  "Cm"           to   ps-lit-scf
                     go to set-hvf-200
           else      go to set-hvf-300.
       set-hvf-200.
      *              *-------------------------------------------------*
      *              * Se scaling factor espresso come literal         *
      *              *-------------------------------------------------*
       set-hvf-210.
      *                  *---------------------------------------------*
      *                  * Linea : SetHorizontalVerticalFactor         *
      *                  *---------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     ps-lit-scf
                                delimited by   spaces
                     ") SetHorizontalVerticalFactor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       set-hvf-220.
      *                  *---------------------------------------------*
      *                  * A rinormalizzazioni finali                  *
      *                  *---------------------------------------------*
           go to     set-hvf-900.
       set-hvf-300.
      *              *-------------------------------------------------*
      *              * Se scaling factor espresso come valore numerico *
      *              * in Pt                                           *
      *              *-------------------------------------------------*
       set-hvf-310.
      *                  *---------------------------------------------*
      *                  * Se scaling factor a zero lo si forza a 1    *
      *                  *---------------------------------------------*
           if        ps-num-scf           =    zero
                     move  1              to   ps-num-scf             .
       set-hvf-320.
      *                  *---------------------------------------------*
      *                  * Linea : SetHorizontalVerticalFactor         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing scaling factor numerico         *
      *                      *-----------------------------------------*
           move      ps-num-scf           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *                      *-----------------------------------------*
      *                      * Linea                                   *
      *                      *-----------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " SetHorizontalVerticalFactor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       set-hvf-900.
      *              *-------------------------------------------------*
      *              * Rinormalizzazioni finali                        *
      *              *-------------------------------------------------*
           move      spaces               to   ps-lit-scf             .
           move      spaces               to   ps-num-scf             .

       set-hvf-999.
           exit.

      *================================================================*
      *    Rotate                                                      *
      *----------------------------------------------------------------*
       rot-ate-000.
      *              *-------------------------------------------------*
      *              * Editing rotation angle                          *
      *              *-------------------------------------------------*
           move      ps-rot-ang           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : Rotate                                  *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " Rotate"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       rot-ate-999.
           exit.

      *================================================================*
      *    Translate                                                   *
      *----------------------------------------------------------------*
       tra-nsl-000.
      *              *-------------------------------------------------*
      *              * Editing horizontal translation                  *
      *              *-------------------------------------------------*
           move      ps-hor-tra           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing vertical translation                    *
      *              *-------------------------------------------------*
           move      ps-ver-tra           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Linea : Translate                               *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " Translate"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       tra-nsl-999.
           exit.

      *================================================================*
      *    Scale                                                       *
      *----------------------------------------------------------------*
       sca-lex-000.
      *              *-------------------------------------------------*
      *              * Editing horizontal scale                        *
      *              *-------------------------------------------------*
           move      ps-hor-sca           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing vertical scale                          *
      *              *-------------------------------------------------*
           move      ps-ver-sca           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Linea : Scale                                   *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " Scale"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       sca-lex-999.
           exit.

      *================================================================*
      *    SetLineWidth                                                *
      *----------------------------------------------------------------*
       lin-wid-000.
      *              *-------------------------------------------------*
      *              * Editing line width                              *
      *              *-------------------------------------------------*
           move      ps-lin-wid           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetLineWidth                            *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " SetLineWidth"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       lin-wid-999.
           exit.

      *================================================================*
      *    SetDash                                                     *
      *----------------------------------------------------------------*
       das-hxx-000.
      *              *-------------------------------------------------*
      *              * Terminazione con high-value del dash pattern    *
      *              *-------------------------------------------------*
           move      ps-das-pat           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetDash                                 *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-ter-str-h01
                                delimited by   high-value
                     " SetDash"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       das-hxx-999.
           exit.

      *================================================================*
      *    SetLineCap                                                  *
      *----------------------------------------------------------------*
       lin-cap-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del parametro       *
      *              *-------------------------------------------------*
           move      ps-lin-cap           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-lin-cap             .
       lin-cap-100.
      *              *-------------------------------------------------*
      *              * Riconoscimento del parametro                    *
      *              *-------------------------------------------------*
           if        ps-lin-cap           =    "BUTT"
                     move  "Butt            "
                                          to   ps-lin-cap
                     go to lin-cap-200
           else if   ps-lin-cap           =    "ROUND"
                     move  "Round           "
                                          to   ps-lin-cap
                     go to lin-cap-200
           else if   ps-lin-cap           =    "PROJECTINGSQUARE"
                     move  "ProjectingSquare"
                                          to   ps-lin-cap
                     go to lin-cap-200
           else      go to lin-cap-999.
       lin-cap-200.
      *              *-------------------------------------------------*
      *              * Linea : SetLineCap                              *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     ps-lin-cap
                                delimited by   spaces
                     ") SetLineCap"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       lin-cap-999.
           exit.

      *================================================================*
      *    SetLineJoin                                                 *
      *----------------------------------------------------------------*
       lin-joi-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del parametro       *
      *              *-------------------------------------------------*
           move      ps-lin-joi           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-lin-joi             .
       lin-joi-100.
      *              *-------------------------------------------------*
      *              * Riconoscimento del parametro                    *
      *              *-------------------------------------------------*
           if        ps-lin-joi           =    "MITER"
                     move  "Miter"        to   ps-lin-joi
                     go to lin-joi-200
           else if   ps-lin-joi           =    "ROUND"
                     move  "Round"        to   ps-lin-joi
                     go to lin-joi-200
           else if   ps-lin-joi           =    "BEVEL"
                     move  "Bevel"        to   ps-lin-joi
                     go to lin-joi-200
           else      go to lin-joi-999.
       lin-joi-200.
      *              *-------------------------------------------------*
      *              * Linea : SetLineJoin                             *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     ps-lin-joi
                                delimited by   spaces
                     ") SetLineJoin"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       lin-joi-999.
           exit.

      *================================================================*
      *    SetMiterLimit                                               *
      *----------------------------------------------------------------*
       mit-lim-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del parametro       *
      *              *-------------------------------------------------*
           move      ps-mit-lim           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-mit-lim             .
       mit-lim-100.
      *              *-------------------------------------------------*
      *              * Riconoscimento del parametro                    *
      *              *-------------------------------------------------*
           if        ps-mit-lim           =    "90DEGREES"
                     move  "90Degrees"    to   ps-mit-lim
                     go to mit-lim-200
           else if   ps-mit-lim           =    "60DEGREES"
                     move  "60Degrees"    to   ps-mit-lim
                     go to mit-lim-200
           else if   ps-mit-lim           =    "11DEGREES"
                     move  "11Degrees"    to   ps-mit-lim
                     go to mit-lim-200
           else if   ps-mit-lim           =    "ALWAYS"
                     move  "Always   "    to   ps-mit-lim
                     go to mit-lim-200
           else      go to mit-lim-999.
       mit-lim-200.
      *              *-------------------------------------------------*
      *              * Linea : SetMiterLimit                           *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     ps-mit-lim
                                delimited by   spaces
                     ") SetMiterLimit"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       mit-lim-999.
           exit.

      *================================================================*
      *    SetStrokeRGBColor                                           *
      *----------------------------------------------------------------*
       str-rgb-000.
      *              *-------------------------------------------------*
      *              * Editing RGB Red                                 *
      *              *-------------------------------------------------*
           move      ps-rgb-red           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing RGB Green                               *
      *              *-------------------------------------------------*
           move      ps-rgb-gre           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Editing RGB Blue                                *
      *              *-------------------------------------------------*
           move      ps-rgb-blu           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetStrokeRGBColor                       *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " SetStrokeRGBColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       str-rgb-999.
           exit.

      *================================================================*
      *    SetStrokeHSBColor                                           *
      *----------------------------------------------------------------*
       str-hsb-000.
      *              *-------------------------------------------------*
      *              * Editing HSB Hue                                 *
      *              *-------------------------------------------------*
           move      ps-hsb-hue           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing HSB Saturation                          *
      *              *-------------------------------------------------*
           move      ps-hsb-sat           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Editing HSB Brightness                          *
      *              *-------------------------------------------------*
           move      ps-hsb-bri           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetStrokeHSBColor                       *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " SetStrokeHSBColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       str-hsb-999.
           exit.

      *================================================================*
      *    SetStrokeCMYKColor                                          *
      *----------------------------------------------------------------*
       str-cmy-000.
      *              *-------------------------------------------------*
      *              * Editing CMYK Cyan                               *
      *              *-------------------------------------------------*
           move      ps-cmy-cya           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing CMYK Magenta                            *
      *              *-------------------------------------------------*
           move      ps-cmy-mag           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Editing CMYK Yellow                             *
      *              *-------------------------------------------------*
           move      ps-cmy-yel           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
      *              *-------------------------------------------------*
      *              * Editing CMYK Black                              *
      *              *-------------------------------------------------*
           move      ps-cmy-bla           to   w-edt-num-num          .
           perform   edt-asx-num-e04-000  thru edt-asx-num-e04-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetStrokeCMYKColor                      *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " SetStrokeCMYKColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       str-cmy-999.
           exit.

      *================================================================*
      *    SetStrokeGrayColor                                          *
      *----------------------------------------------------------------*
       str-gra-000.
      *              *-------------------------------------------------*
      *              * Editing Gray Black                              *
      *              *-------------------------------------------------*
           move      ps-gra-bla           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetStrokeGrayColor                      *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " SetStrokeGrayColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       str-gra-999.
           exit.

      *================================================================*
      *    SetStrokeColor                                              *
      *----------------------------------------------------------------*
       str-col-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del parametro per   *
      *              * il color name                                   *
      *              *-------------------------------------------------*
           move      ps-col-nam           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-col-nam             .
       str-col-100.
      *              *-------------------------------------------------*
      *              * Riconoscimento del color name                   *
      *              *-------------------------------------------------*
           if        ps-col-nam           =    "BLACK  "
                     move  "Black  "      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "RED    "
                     move  "Red    "      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "GREEN  "
                     move  "Green  "      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "BLU    "
                     move  "Blu     "     to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "YELLOW "
                     move  "Yellow "      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "MAGENTA"
                     move  "Magenta"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "CYAN   "
                     move  "Cyan   "      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "WHITE  "
                     move  "White  "      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "5%GRAY "
                     move  "5%Gray "      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "10%GRAY"
                     move  "10%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "15%GRAY"
                     move  "15%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "20%GRAY"
                     move  "20%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "25%GRAY"
                     move  "25%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "30%GRAY"
                     move  "30%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "35%GRAY"
                     move  "35%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "40%GRAY"
                     move  "40%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "45%GRAY"
                     move  "45%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "50%GRAY"
                     move  "50%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "55%GRAY"
                     move  "55%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "60%GRAY"
                     move  "60%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "65%GRAY"
                     move  "65%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "70%GRAY"
                     move  "70%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "75%GRAY"
                     move  "75%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "80%GRAY"
                     move  "80%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "85%GRAY"
                     move  "85%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "90%GRAY"
                     move  "90%Gray"      to   ps-col-nam
                     go to str-col-200
           else if   ps-col-nam           =    "95%GRAY"
                     move  "95%Gray"      to   ps-col-nam
                     go to str-col-200
           else      go to str-col-999.
       str-col-200.
      *              *-------------------------------------------------*
      *              * Linea : SetStrokeColor                          *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     ps-col-nam
                                delimited by   spaces
                     ") SetStrokeColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       str-col-999.
           exit.

      *================================================================*
      *    SetFillRGBColor                                             *
      *----------------------------------------------------------------*
       fil-rgb-000.
      *              *-------------------------------------------------*
      *              * Editing RGB Red                                 *
      *              *-------------------------------------------------*
           move      ps-rgb-red           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing RGB Green                               *
      *              *-------------------------------------------------*
           move      ps-rgb-gre           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Editing RGB Blue                                *
      *              *-------------------------------------------------*
           move      ps-rgb-blu           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetFillRGBColor                         *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " SetFillRGBColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       fil-rgb-999.
           exit.

      *================================================================*
      *    SetFillHSBColor                                             *
      *----------------------------------------------------------------*
       fil-hsb-000.
      *              *-------------------------------------------------*
      *              * Editing HSB Hue                                 *
      *              *-------------------------------------------------*
           move      ps-hsb-hue           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing HSB Saturation                          *
      *              *-------------------------------------------------*
           move      ps-hsb-sat           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Editing HSB Brightness                          *
      *              *-------------------------------------------------*
           move      ps-hsb-bri           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetFillHSBColor                         *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " SetFillHSBColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       fil-hsb-999.
           exit.

      *================================================================*
      *    SetFillCMYKColor                                            *
      *----------------------------------------------------------------*
       fil-cmy-000.
      *              *-------------------------------------------------*
      *              * Editing CMYK Cyan                               *
      *              *-------------------------------------------------*
           move      ps-cmy-cya           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing CMYK Magenta                            *
      *              *-------------------------------------------------*
           move      ps-cmy-mag           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Editing CMYK Yellow                             *
      *              *-------------------------------------------------*
           move      ps-cmy-yel           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
      *              *-------------------------------------------------*
      *              * Editing CMYK Black                              *
      *              *-------------------------------------------------*
           move      ps-cmy-bla           to   w-edt-num-num          .
           perform   edt-asx-num-e04-000  thru edt-asx-num-e04-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetFillCMYKColor                        *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " SetFillCMYKColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       fil-cmy-999.
           exit.

      *================================================================*
      *    SetFillGrayColor                                            *
      *----------------------------------------------------------------*
       fil-gra-000.
      *              *-------------------------------------------------*
      *              * Editing Gray Black                              *
      *              *-------------------------------------------------*
           move      ps-gra-bla           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetFillGrayColor                        *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " SetFillGrayColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       fil-gra-999.
           exit.

      *================================================================*
      *    SetFillColor                                                *
      *----------------------------------------------------------------*
       fil-col-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del parametro per   *
      *              * il color name                                   *
      *              *-------------------------------------------------*
           move      ps-col-nam           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-col-nam             .
       fil-col-100.
      *              *-------------------------------------------------*
      *              * Riconoscimento del color name                   *
      *              *-------------------------------------------------*
           if        ps-col-nam           =    "BLACK  "
                     move  "Black  "      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "RED    "
                     move  "Red    "      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "GREEN  "
                     move  "Green  "      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "BLU    "
                     move  "Blu     "     to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "YELLOW "
                     move  "Yellow "      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "MAGENTA"
                     move  "Magenta"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "CYAN   "
                     move  "Cyan   "      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "WHITE  "
                     move  "White  "      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "5%GRAY "
                     move  "5%Gray "      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "10%GRAY"
                     move  "10%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "15%GRAY"
                     move  "15%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "20%GRAY"
                     move  "20%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "25%GRAY"
                     move  "25%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "30%GRAY"
                     move  "30%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "35%GRAY"
                     move  "35%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "40%GRAY"
                     move  "40%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "45%GRAY"
                     move  "45%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "50%GRAY"
                     move  "50%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "55%GRAY"
                     move  "55%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "60%GRAY"
                     move  "60%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "65%GRAY"
                     move  "65%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "70%GRAY"
                     move  "70%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "75%GRAY"
                     move  "75%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "80%GRAY"
                     move  "80%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "85%GRAY"
                     move  "85%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "90%GRAY"
                     move  "90%Gray"      to   ps-col-nam
                     go to fil-col-200
           else if   ps-col-nam           =    "95%GRAY"
                     move  "95%Gray"      to   ps-col-nam
                     go to fil-col-200
           else      go to fil-col-999.
       fil-col-200.
      *              *-------------------------------------------------*
      *              * Linea : SetFillColor                            *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     ps-col-nam
                                delimited by   spaces
                     ") SetFillColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       fil-col-999.
           exit.

      *================================================================*
      *    SetTextRGBColor                                             *
      *----------------------------------------------------------------*
       txt-rgb-000.
      *              *-------------------------------------------------*
      *              * Editing RGB Red                                 *
      *              *-------------------------------------------------*
           move      ps-rgb-red           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing RGB Green                               *
      *              *-------------------------------------------------*
           move      ps-rgb-gre           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Editing RGB Blue                                *
      *              *-------------------------------------------------*
           move      ps-rgb-blu           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetTextRGBColor                         *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " SetTextRGBColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       txt-rgb-999.
           exit.

      *================================================================*
      *    SetTextHSBColor                                             *
      *----------------------------------------------------------------*
       txt-hsb-000.
      *              *-------------------------------------------------*
      *              * Editing HSB Hue                                 *
      *              *-------------------------------------------------*
           move      ps-hsb-hue           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing HSB Saturation                          *
      *              *-------------------------------------------------*
           move      ps-hsb-sat           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Editing HSB Brightness                          *
      *              *-------------------------------------------------*
           move      ps-hsb-bri           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetTextHSBColor                         *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " SetTextHSBColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       txt-hsb-999.
           exit.

      *================================================================*
      *    SetTextCMYKColor                                            *
      *----------------------------------------------------------------*
       txt-cmy-000.
      *              *-------------------------------------------------*
      *              * Editing CMYK Cyan                               *
      *              *-------------------------------------------------*
           move      ps-cmy-cya           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Editing CMYK Magenta                            *
      *              *-------------------------------------------------*
           move      ps-cmy-mag           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Editing CMYK Yellow                             *
      *              *-------------------------------------------------*
           move      ps-cmy-yel           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
      *              *-------------------------------------------------*
      *              * Editing CMYK Black                              *
      *              *-------------------------------------------------*
           move      ps-cmy-bla           to   w-edt-num-num          .
           perform   edt-asx-num-e04-000  thru edt-asx-num-e04-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetTextCMYKColor                        *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " SetTextCMYKColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       txt-cmy-999.
           exit.

      *================================================================*
      *    SetTextGrayColor                                            *
      *----------------------------------------------------------------*
       txt-gra-000.
      *              *-------------------------------------------------*
      *              * Editing Gray Black                              *
      *              *-------------------------------------------------*
           move      ps-gra-bla           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetTextGrayColor                        *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " SetTextGrayColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       txt-gra-999.
           exit.

      *================================================================*
      *    SetTextColor                                                *
      *----------------------------------------------------------------*
       txt-col-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del parametro per   *
      *              * il color name                                   *
      *              *-------------------------------------------------*
           move      ps-col-nam           to   w-upp-cas-str          .
           perform   tra-str-upp-cas-000  thru tra-str-upp-cas-999    .
           move      w-upp-cas-str        to   ps-col-nam             .
       txt-col-100.
      *              *-------------------------------------------------*
      *              * Riconoscimento del color name                   *
      *              *-------------------------------------------------*
           if        ps-col-nam           =    "BLACK  "
                     move  "Black  "      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "RED    "
                     move  "Red    "      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "GREEN  "
                     move  "Green  "      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "BLU    "
                     move  "Blu     "     to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "YELLOW "
                     move  "Yellow "      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "MAGENTA"
                     move  "Magenta"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "CYAN   "
                     move  "Cyan   "      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "WHITE  "
                     move  "White  "      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "5%GRAY "
                     move  "5%Gray "      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "10%GRAY"
                     move  "10%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "15%GRAY"
                     move  "15%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "20%GRAY"
                     move  "20%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "25%GRAY"
                     move  "25%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "30%GRAY"
                     move  "30%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "35%GRAY"
                     move  "35%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "40%GRAY"
                     move  "40%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "45%GRAY"
                     move  "45%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "50%GRAY"
                     move  "50%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "55%GRAY"
                     move  "55%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "60%GRAY"
                     move  "60%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "65%GRAY"
                     move  "65%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "70%GRAY"
                     move  "70%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "75%GRAY"
                     move  "75%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "80%GRAY"
                     move  "80%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "85%GRAY"
                     move  "85%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "90%GRAY"
                     move  "90%Gray"      to   ps-col-nam
                     go to txt-col-200
           else if   ps-col-nam           =    "95%GRAY"
                     move  "95%Gray"      to   ps-col-nam
                     go to txt-col-200
           else      go to txt-col-999.
       txt-col-200.
      *              *-------------------------------------------------*
      *              * Linea : SetTextColor                            *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     ps-col-nam
                                delimited by   spaces
                     ") SetTextColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       txt-col-999.
           exit.

      *================================================================*
      *    SetRoundingRadius                                           *
      *----------------------------------------------------------------*
       rou-rad-000.
      *              *-------------------------------------------------*
      *              * Editing del rounding radius                     *
      *              *-------------------------------------------------*
           move      ps-rou-rad           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetRoundingRadius                       *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                     " SetRoundingRadius"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       rou-rad-999.
           exit.

      *================================================================*
      *    SetShadingOffset                                            *
      *----------------------------------------------------------------*
       sha-off-000.
      *              *-------------------------------------------------*
      *              * Editing del shading offset                      *
      *              *-------------------------------------------------*
           move      ps-sha-off           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetShadingOffset                        *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                     " SetShadingOffset"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       sha-off-999.
           exit.

      *================================================================*
      *    SetShadingGrayColor                                         *
      *----------------------------------------------------------------*
       sha-gra-000.
      *              *-------------------------------------------------*
      *              * Editing Gray Black                              *
      *              *-------------------------------------------------*
           move      ps-sha-bla           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : SetShadingGrayColor                     *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " SetShadingGrayColor"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       sha-gra-999.
           exit.

      *================================================================*
      *    SelectFont                                                  *
      *----------------------------------------------------------------*
       sel-fon-000.
      *              *-------------------------------------------------*
      *              * Terminazione con high-value del font name       *
      *              *-------------------------------------------------*
           move      ps-fon-nam           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Editing del font size                           *
      *              *-------------------------------------------------*
           move      ps-fon-siz           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
      *              *-------------------------------------------------*
      *              * Linea : SelectFont                              *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    "("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ") "
                                delimited by   size
                     w-edt-num-e01
                                delimited by   spaces
                     " "
                     " SelectFont"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       sel-fon-999.
           exit.

      *================================================================*
      *    Line                                                        *
      *----------------------------------------------------------------*
       lin-exx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Linea : Line                                    *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " Line"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       lin-exx-999.
           exit.

      *================================================================*
      *    RectStroke                                                  *
      *----------------------------------------------------------------*
       rec-str-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Linea : RectStroke                              *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " RectStroke"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       rec-str-999.
           exit.

      *================================================================*
      *    RectFill                                                    *
      *----------------------------------------------------------------*
       rec-fil-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Linea : RectFill                                *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " RectFill"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       rec-fil-999.
           exit.

      *================================================================*
      *    Rect                                                        *
      *----------------------------------------------------------------*
       rec-txx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Linea : Rect                                    *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " Rect"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       rec-txx-999.
           exit.

      *================================================================*
      *    RoundRectStroke                                             *
      *----------------------------------------------------------------*
       rrc-str-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Linea : RoundRectStroke                         *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " RoundRectStroke"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       rrc-str-999.
           exit.

      *================================================================*
      *    RoundRectFill                                               *
      *----------------------------------------------------------------*
       rrc-fil-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Linea : RoundRectFill                           *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " RoundRectFill"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       rrc-fil-999.
           exit.

      *================================================================*
      *    RoundRect                                                   *
      *----------------------------------------------------------------*
       rrc-txx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Linea : RoundRect                               *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " RoundRect"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       rrc-txx-999.
           exit.

      *================================================================*
      *    ShadowRect                                                  *
      *----------------------------------------------------------------*
       sha-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Linea : ShadowRect                              *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " ShadowRect"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       sha-rec-999.
           exit.

      *================================================================*
      *    ShadowRoundRect                                             *
      *----------------------------------------------------------------*
       sha-rrc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Linea : ShadowRoundRect                         *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " ShadowRoundRect"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       sha-rrc-999.
           exit.

      *================================================================*
      *    BaseLPrint                                                  *
      *----------------------------------------------------------------*
       bas-lpr-000.
      *              *-------------------------------------------------*
      *              * Editing coordinate ps-coo-xxx e ps-coo-yyy      *
      *              *-------------------------------------------------*
           move      ps-coo-xxx           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
           move      ps-coo-yyy           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : BaseLPrint                              *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     "("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " BaseLPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       bas-lpr-999.
           exit.

      *================================================================*
      *    BaseCPrint                                                  *
      *----------------------------------------------------------------*
       bas-cpr-000.
      *              *-------------------------------------------------*
      *              * Editing coordinate ps-coo-xxx e ps-coo-yyy      *
      *              *-------------------------------------------------*
           move      ps-coo-xxx           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
           move      ps-coo-yyy           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : BaseCPrint                              *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     "("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " BaseCPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       bas-cpr-999.
           exit.

      *================================================================*
      *    BaseRPrint                                                  *
      *----------------------------------------------------------------*
       bas-rpr-000.
      *              *-------------------------------------------------*
      *              * Editing coordinate ps-coo-xxx e ps-coo-yyy      *
      *              *-------------------------------------------------*
           move      ps-coo-xxx           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
           move      ps-coo-yyy           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : BaseRPrint                              *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     "("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " BaseRPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       bas-rpr-999.
           exit.

      *================================================================*
      *    LPrint                                                      *
      *----------------------------------------------------------------*
       lpr-int-000.
      *              *-------------------------------------------------*
      *              * Editing coordinate ps-coo-xxx e ps-coo-yyy      *
      *              *-------------------------------------------------*
           move      ps-coo-xxx           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
           move      ps-coo-yyy           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : LPrint                                  *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     "("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " LPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       lpr-int-999.
           exit.

      *================================================================*
      *    CPrint                                                      *
      *----------------------------------------------------------------*
       cpr-int-000.
      *              *-------------------------------------------------*
      *              * Editing coordinate ps-coo-xxx e ps-coo-yyy      *
      *              *-------------------------------------------------*
           move      ps-coo-xxx           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
           move      ps-coo-yyy           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : CPrint                                  *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     "("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " CPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       cpr-int-999.
           exit.

      *================================================================*
      *    RPrint                                                      *
      *----------------------------------------------------------------*
       rpr-int-000.
      *              *-------------------------------------------------*
      *              * Editing coordinate ps-coo-xxx e ps-coo-yyy      *
      *              *-------------------------------------------------*
           move      ps-coo-xxx           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
           move      ps-coo-yyy           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : RPrint                                  *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     "("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " RPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       rpr-int-999.
           exit.

      *================================================================*
      *    BoxBaseLPrint                                               *
      *----------------------------------------------------------------*
       bxb-lpr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : BoxBaseLPrint                           *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " ("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " BoxBaseLPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       bxb-lpr-999.
           exit.

      *================================================================*
      *    BoxBaseCPrint                                               *
      *----------------------------------------------------------------*
       bxb-cpr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : BoxBaseCPrint                           *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " ("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " BoxBaseCPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       bxb-cpr-999.
           exit.

      *================================================================*
      *    BoxBaseRPrint                                               *
      *----------------------------------------------------------------*
       bxb-rpr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : BoxBaseRPrint                           *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " ("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " BoxBaseRPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       bxb-rpr-999.
           exit.

      *================================================================*
      *    BoxLPrint                                                   *
      *----------------------------------------------------------------*
       box-lpr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : BoxLPrint                               *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " ("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " BoxLPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       box-lpr-999.
           exit.

      *================================================================*
      *    BoxCPrint                                                   *
      *----------------------------------------------------------------*
       box-cpr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : BoxCPrint                               *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " ("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " BoxCPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       box-cpr-999.
           exit.

      *================================================================*
      *    BoxRPrint                                                   *
      *----------------------------------------------------------------*
       box-rpr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ed editing coordinate rettango- *
      *              * lari                                            *
      *              *-------------------------------------------------*
           perform   nor-edt-coo-ret-000  thru nor-edt-coo-ret-999    .
      *              *-------------------------------------------------*
      *              * Terminazione con high-value della stringa       *
      *              *-------------------------------------------------*
           move      ps-str-alf           to   w-ter-str-alf          .
           perform   ter-str-alf-h01-000  thru ter-str-alf-h01-999    .
      *              *-------------------------------------------------*
      *              * Linea : BoxRPrint                               *
      *              *-------------------------------------------------*
           move      spaces               to   stp-rec                .
           string    w-edt-num-e01
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e02
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e03
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-edt-num-e04
                                delimited by   spaces
                     " ("
                                delimited by   size
                     w-ter-str-h01
                                delimited by   high-value
                     ")"
                                delimited by   size
                     " BoxRPrint"
                                delimited by   size
                                          into stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       box-rpr-999.
           exit.

      *================================================================*
      *    IncludeEPS                                                  *
      *----------------------------------------------------------------*
       inc-eps-000.
      *              *-------------------------------------------------*
      *              * Linea : gsave                                   *
      *              *-------------------------------------------------*
           move      "gsave"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%BeginDocument                         *
      *              *-------------------------------------------------*
           move      "%%BeginDocument"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Append del file EPS al file [stp]               *
      *              *-------------------------------------------------*
           move      ps-eps-pat           to   w-trt-fil-eps-pat      .
           perform   apd-eps-fil-stp-000  thru apd-eps-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : %%EndDocument                           *
      *              *-------------------------------------------------*
           move      "%%EndDocument"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Linea : grestore                                *
      *              *-------------------------------------------------*
           move      "grestore"
                                          to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       inc-eps-999.
           exit.

      *================================================================*
      *    PostScript                                                  *
      *----------------------------------------------------------------*
       pos-scr-000.
      *              *-------------------------------------------------*
      *              * Scrittura di tutte le linee, da ps-pos-001 a    *
      *              * ps-pos-010, tralasciando quelle completamente   *
      *              * a spaces                                        *
      *              *-------------------------------------------------*
       pos-scr-050.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-001                            *
      *                  *---------------------------------------------*
           if        ps-pos-001           =    spaces
                     go to pos-scr-100.
           move      ps-pos-001           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-100.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-002                            *
      *                  *---------------------------------------------*
           if        ps-pos-002           =    spaces
                     go to pos-scr-150.
           move      ps-pos-002           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-150.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-003                            *
      *                  *---------------------------------------------*
           if        ps-pos-003           =    spaces
                     go to pos-scr-200.
           move      ps-pos-003           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-200.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-004                            *
      *                  *---------------------------------------------*
           if        ps-pos-004           =    spaces
                     go to pos-scr-250.
           move      ps-pos-004           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-250.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-005                            *
      *                  *---------------------------------------------*
           if        ps-pos-005           =    spaces
                     go to pos-scr-300.
           move      ps-pos-005           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-300.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-006                            *
      *                  *---------------------------------------------*
           if        ps-pos-006           =    spaces
                     go to pos-scr-350.
           move      ps-pos-006           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-350.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-007                            *
      *                  *---------------------------------------------*
           if        ps-pos-007           =    spaces
                     go to pos-scr-400.
           move      ps-pos-007           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-400.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-008                            *
      *                  *---------------------------------------------*
           if        ps-pos-008           =    spaces
                     go to pos-scr-450.
           move      ps-pos-008           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-450.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-009                            *
      *                  *---------------------------------------------*
           if        ps-pos-009           =    spaces
                     go to pos-scr-500.
           move      ps-pos-009           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-500.
      *                  *---------------------------------------------*
      *                  * Linea ps-pos-010                            *
      *                  *---------------------------------------------*
           if        ps-pos-010           =    spaces
                     go to pos-scr-550.
           move      ps-pos-010           to   stp-rec                .
           perform   wrt-rec-fil-stp-000  thru wrt-rec-fil-stp-999    .
       pos-scr-550.
      *              *-------------------------------------------------*
      *              * Rinormalizzazione di tutte le linee             *
      *              *-------------------------------------------------*
           move      spaces               to   ps-pos-001             .
           move      spaces               to   ps-pos-002             .
           move      spaces               to   ps-pos-003             .
           move      spaces               to   ps-pos-004             .
           move      spaces               to   ps-pos-005             .
           move      spaces               to   ps-pos-006             .
           move      spaces               to   ps-pos-007             .
           move      spaces               to   ps-pos-008             .
           move      spaces               to   ps-pos-009             .
           move      spaces               to   ps-pos-010             .
       pos-scr-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pos-scr-999.
       pos-scr-999.
           exit.

      *    *===========================================================*
      *    * Trasformazione in uppercase del campo w-upp-cas-str       *
      *    *-----------------------------------------------------------*
       tra-str-upp-cas-000.
           move      zero                 to   w-upp-cas-c01          .
           inspect   w-upp-cas-str    tallying w-upp-cas-c01
                                  for trailing spaces                 .
           move      240                  to   w-upp-cas-c00          .
           subtract  w-upp-cas-c01        from w-upp-cas-c00          .
           move      zero                 to   w-upp-cas-c01          .
       tra-str-upp-cas-100.
           add       1                    to   w-upp-cas-c01          .
           if        w-upp-cas-c01        >    w-upp-cas-c00
                     go to tra-str-upp-cas-999.
           move      zero                 to   w-upp-cas-c02          .
           inspect   w-upp-cas-low    tallying w-upp-cas-c02
                     for characters     before
                                       initial w-upp-cas-chr
                                              (w-upp-cas-c01)         .
           if        w-upp-cas-c02        not  < 26
                     go to tra-str-upp-cas-100.
           add       1                    to   w-upp-cas-c02          .
           move      w-upp-cas-upp
                    (w-upp-cas-c02: 1)    to   w-upp-cas-chr
                                              (w-upp-cas-c01)         .
           go to     tra-str-upp-cas-100.
       tra-str-upp-cas-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione ed editing coordinate rettangolari,       *
      *    * ps-coo-xxx, ps-coo-yyy, ps-coo-xto e ps-coo-yto, in modo  *
      *    * che xxx sia piu' a sinistra di xto e yyy sia piu' in      *
      *    * basso di pto                                              *
      *    *-----------------------------------------------------------*
       nor-edt-coo-ret-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           if        ps-coo-xxx           not  > ps-coo-xto and
                     ps-coo-yyy           not  > ps-coo-yto
                     go to nor-edt-coo-ret-500.
           move      ps-coo-xxx           to   w-exe-job-cal-c01      .
           move      ps-coo-yyy           to   w-exe-job-cal-c02      .
           move      ps-coo-xto           to   w-exe-job-cal-c03      .
           move      ps-coo-yto           to   w-exe-job-cal-c04      .
           if        ps-coo-xxx           not  > ps-coo-xto and
                     ps-coo-yyy           >    ps-coo-yto
                     move  w-exe-job-cal-c02
                                          to   ps-coo-yto
                     move  w-exe-job-cal-c04
                                          to   ps-coo-yyy
                     go to nor-edt-coo-ret-500.
           if        ps-coo-xxx           >    ps-coo-xto and
                     ps-coo-yyy           not  > ps-coo-yto
                     move  w-exe-job-cal-c01
                                          to   ps-coo-xto
                     move  w-exe-job-cal-c03
                                          to   ps-coo-xxx
                     go to nor-edt-coo-ret-500.
           move      w-exe-job-cal-c01    to   ps-coo-xto             .
           move      w-exe-job-cal-c03    to   ps-coo-xxx             .
           move      w-exe-job-cal-c02    to   ps-coo-yto             .
           move      w-exe-job-cal-c04    to   ps-coo-yyy             .
       nor-edt-coo-ret-500.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      ps-coo-xxx           to   w-edt-num-num          .
           perform   edt-asx-num-e01-000  thru edt-asx-num-e01-999    .
           move      ps-coo-yyy           to   w-edt-num-num          .
           perform   edt-asx-num-e02-000  thru edt-asx-num-e02-999    .
           move      ps-coo-xto           to   w-edt-num-num          .
           perform   edt-asx-num-e03-000  thru edt-asx-num-e03-999    .
           move      ps-coo-yto           to   w-edt-num-num          .
           perform   edt-asx-num-e04-000  thru edt-asx-num-e04-999    .
       nor-edt-coo-ret-999.
           exit.

      *    *===========================================================*
      *    * Editing del valore numerico w-edt-num-num allineandolo    *
      *    * a sinistra in w-edt-num-e01                               *
      *    *-----------------------------------------------------------*
       edt-asx-num-e01-000.
           perform   edt-asx-num-exx-000  thru edt-asx-num-exx-999    .
           move      w-edt-num-exx        to   w-edt-num-e01          .
       edt-asx-num-e01-999.
           exit.

      *    *===========================================================*
      *    * Editing del valore numerico w-edt-num-num allineandolo    *
      *    * a sinistra in w-edt-num-e02                               *
      *    *-----------------------------------------------------------*
       edt-asx-num-e02-000.
           perform   edt-asx-num-exx-000  thru edt-asx-num-exx-999    .
           move      w-edt-num-exx        to   w-edt-num-e02          .
       edt-asx-num-e02-999.
           exit.

      *    *===========================================================*
      *    * Editing del valore numerico w-edt-num-num allineandolo    *
      *    * a sinistra in w-edt-num-e03                               *
      *    *-----------------------------------------------------------*
       edt-asx-num-e03-000.
           perform   edt-asx-num-exx-000  thru edt-asx-num-exx-999    .
           move      w-edt-num-exx        to   w-edt-num-e03          .
       edt-asx-num-e03-999.
           exit.

      *    *===========================================================*
      *    * Editing del valore numerico w-edt-num-num allineandolo    *
      *    * a sinistra in w-edt-num-e04                               *
      *    *-----------------------------------------------------------*
       edt-asx-num-e04-000.
           perform   edt-asx-num-exx-000  thru edt-asx-num-exx-999    .
           move      w-edt-num-exx        to   w-edt-num-e04          .
       edt-asx-num-e04-999.
           exit.

      *    *===========================================================*
      *    * Editing del valore numerico w-edt-num-num allineandolo    *
      *    * a sinistra in w-edt-num-e05                               *
      *    *-----------------------------------------------------------*
       edt-asx-num-e05-000.
           perform   edt-asx-num-exx-000  thru edt-asx-num-exx-999    .
           move      w-edt-num-exx        to   w-edt-num-e05          .
       edt-asx-num-e05-999.
           exit.

      *    *===========================================================*
      *    * Editing del valore numerico w-edt-num-num allineandolo    *
      *    * a sinistra in w-edt-num-e06                               *
      *    *-----------------------------------------------------------*
       edt-asx-num-e06-000.
           perform   edt-asx-num-exx-000  thru edt-asx-num-exx-999    .
           move      w-edt-num-exx        to   w-edt-num-e06          .
       edt-asx-num-e06-999.
           exit.

      *    *===========================================================*
      *    * Editing del valore numerico w-edt-num-num allineandolo    *
      *    * a sinistra in w-edt-num-exx                               *
      *    *-----------------------------------------------------------*
       edt-asx-num-exx-000.
           move      w-edt-num-num        to   w-edt-num-ned          .
           move      11                   to   w-edt-num-i02          .
       edt-asx-num-exx-100.
           subtract  1                    from w-edt-num-i02          .
           if        w-edt-num-chr
                    (w-edt-num-i02)       =    "0"
                     move  spaces         to   w-edt-num-chr
                                              (w-edt-num-i02)
                     go to edt-asx-num-exx-100.
           if        w-edt-num-chr
                    (w-edt-num-i02)       =    ","
                     move  spaces         to   w-edt-num-chr
                                              (w-edt-num-i02)
                     subtract   1         from w-edt-num-i02          .
       edt-asx-num-exx-200.
           move      1                    to   w-edt-num-i01          .
       edt-asx-num-exx-300.
           add       1                    to   w-edt-num-i01          .
           if        w-edt-num-chr
                    (w-edt-num-i01)       =    spaces
                     go to edt-asx-num-exx-300.
       edt-asx-num-exx-400.
           subtract  w-edt-num-i01        from w-edt-num-i02          .
           add       1                    to   w-edt-num-i02          .
           if        w-edt-num-chr (1)    =    "-"
                     go to edt-asx-num-exx-600.
       edt-asx-num-exx-500.
           move      w-edt-num-eyy
                    (w-edt-num-i01 :
                     w-edt-num-i02 )      to   w-edt-num-exx          .
           go to     edt-asx-num-exx-900.
       edt-asx-num-exx-600.
           move      spaces               to   w-edt-num-exx          .
           string    "-"
                                delimited by   size
                     w-edt-num-eyy
                    (w-edt-num-i01 :
                     w-edt-num-i02 )
                                delimited by   size
                                          into w-edt-num-exx          .
       edt-asx-num-exx-900.
           inspect   w-edt-num-exx   replacing all "," by "."         .
       edt-asx-num-exx-999.
           exit.

      *    *===========================================================*
      *    * Terminazione della stringa contenuta in w-ter-str-alf con *
      *    * il carattere high-value in w-ter-str-h01                  *
      *    *-----------------------------------------------------------*
       ter-str-alf-h01-000.
           perform   ter-str-alf-hxx-000  thru ter-str-alf-hxx-999    .
           move      w-ter-str-hxx        to   w-ter-str-h01          .
       ter-str-alf-h01-999.
           exit.

      *    *===========================================================*
      *    * Terminazione della stringa contenuta in w-ter-str-alf con *
      *    * il carattere high-value in w-ter-str-h02                  *
      *    *-----------------------------------------------------------*
       ter-str-alf-h02-000.
           perform   ter-str-alf-hxx-000  thru ter-str-alf-hxx-999    .
           move      w-ter-str-hxx        to   w-ter-str-h02          .
       ter-str-alf-h02-999.
           exit.

      *    *===========================================================*
      *    * Terminazione della stringa contenuta in w-ter-str-alf con *
      *    * il carattere high-value in w-ter-str-h03                  *
      *    *-----------------------------------------------------------*
       ter-str-alf-h03-000.
           perform   ter-str-alf-hxx-000  thru ter-str-alf-hxx-999    .
           move      w-ter-str-hxx        to   w-ter-str-h03          .
       ter-str-alf-h03-999.
           exit.

      *    *===========================================================*
      *    * Terminazione della stringa contenuta in w-ter-str-alf con *
      *    * il carattere high-value in w-ter-str-h04                  *
      *    *-----------------------------------------------------------*
       ter-str-alf-h04-000.
           perform   ter-str-alf-hxx-000  thru ter-str-alf-hxx-999    .
           move      w-ter-str-hxx        to   w-ter-str-h04          .
       ter-str-alf-h04-999.
           exit.

      *    *===========================================================*
      *    * Terminazione della stringa contenuta in w-ter-str-alf con *
      *    * il carattere high-value in w-ter-str-h05                  *
      *    *-----------------------------------------------------------*
       ter-str-alf-h05-000.
           perform   ter-str-alf-hxx-000  thru ter-str-alf-hxx-999    .
           move      w-ter-str-hxx        to   w-ter-str-h05          .
       ter-str-alf-h05-999.
           exit.

      *    *===========================================================*
      *    * Terminazione della stringa contenuta in w-ter-str-alf con *
      *    * il carattere high-value in w-ter-str-h06                  *
      *    *-----------------------------------------------------------*
       ter-str-alf-h06-000.
           perform   ter-str-alf-hxx-000  thru ter-str-alf-hxx-999    .
           move      w-ter-str-hxx        to   w-ter-str-h06          .
       ter-str-alf-h06-999.
           exit.

      *    *===========================================================*
      *    * Terminazione della stringa contenuta in w-ter-str-alf con *
      *    * il carattere high-value in w-ter-str-hxx                  *
      *    *-----------------------------------------------------------*
       ter-str-alf-hxx-000.
           move      w-ter-str-alf        to   w-ter-str-hxx          .
           move      zero                 to   w-ter-str-i01          .
           inspect   w-ter-str-hxx    tallying w-ter-str-i01
                                  for trailing spaces                 .
           if        w-ter-str-i01        =    zero
                     go to ter-str-alf-hxx-999.
           move      180                  to   w-ter-str-i02          .
           subtract  w-ter-str-i01        from w-ter-str-i02          .
           add       1                    to   w-ter-str-i02          .
           move      high-value           to   w-ter-str-hyy
                                              (w-ter-str-i02)         .
       ter-str-alf-hxx-999.
           exit.

      *    *===========================================================*
      *    * Costruzione del comando per lo spooler di stampa in base  *
      *    * ai seguenti parametri :                                   *
      *    *                                                           *
      *    * - w-sys-spl-cmd-tem : Template per lo spooler             *
      *    * - w-sys-spl-cmd-cst : Codice della stampante              *
      *    * - w-sys-spl-cmd-pat : Pathname del file da stampare       *
      *    * - w-sys-spl-cmd-roe : Redirezione output ed errori        *
      *    *                                                           *
      *    * ponendo il risultato in :                                 *
      *    *                                                           *
      *    * - o-shs : area parametri per chiamate al sistema          *
      *    *-----------------------------------------------------------*
       def-cll-sys-spc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
       def-cll-sys-spc-020.
      *                  *---------------------------------------------*
      *                  * Area o-shs                                  *
      *                  *---------------------------------------------*
           move      spaces               to   o-shs                  .
       def-cll-sys-spc-040.
      *                  *---------------------------------------------*
      *                  * Contatori, indici, puntatori locali         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Indice su template                      *
      *                      *-----------------------------------------*
           move      001                  to   w-sys-spl-inx-tem      .
      *                      *-----------------------------------------*
      *                      * Indice su area o-shs                    *
      *                      *-----------------------------------------*
           move      001                  to   w-sys-spl-inx-hpr      .
       def-cll-sys-spc-050.
      *                  *---------------------------------------------*
      *                  * Controllo che esistano due e solo due ca-   *
      *                  * ratteri '@'                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-sys-spl-ctr-001      .
           inspect   w-sys-spl-cmd-tem
                                      tallying w-sys-spl-ctr-001
                                          for  all   "@"              .
           if        w-sys-spl-ctr-001    not  = 2
                     move  w-sys-spl-cmd-tem
                                          to   o-shs
                     go to def-cll-sys-spc-100.
       def-cll-sys-spc-100.
      *              *-------------------------------------------------*
      *              * Copiatura porzione precedente il primo caratte- *
      *              * re '@'                                          *
      *              *-------------------------------------------------*
       def-cll-sys-spc-120.
      *                  *---------------------------------------------*
      *                  * Determinazione numero di caretteri che pre- *
      *                  * cedono il primo carattere '@'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-sys-spl-ctr-001      .
           inspect   w-sys-spl-cmd-tem
                                      tallying w-sys-spl-ctr-001
                                          for  characters
                                        before initial "@"            .
       def-cll-sys-spc-140.
      *                  *---------------------------------------------*
      *                  * Copiatura                                   *
      *                  *---------------------------------------------*
           move      w-sys-spl-cmd-tem
                    (w-sys-spl-inx-tem:
                     w-sys-spl-ctr-001)   to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
       def-cll-sys-spc-160.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su area o-shs          *
      *                  *---------------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
       def-cll-sys-spc-200.
      *              *-------------------------------------------------*
      *              * Composizione della porzione relativa al primo   *
      *              * carattere '@'                                   *
      *              *-------------------------------------------------*
       def-cll-sys-spc-220.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su template            *
      *                  *---------------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-tem      .
           add       001                  to   w-sys-spl-inx-tem      .
       def-cll-sys-spc-240.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di parame-  *
      *                  * tro                                         *
      *                  *---------------------------------------------*
           if        w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "p" or
                     w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "P"
                     go to def-cll-sys-spc-260
           else if   w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "f" or
                     w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "F"
                     go to def-cll-sys-spc-280
           else      go to def-cll-sys-spc-300.
       def-cll-sys-spc-260.
      *                  *---------------------------------------------*
      *                  * Se codice stampante                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero di caratteri ef-  *
      *                      * fettivi per il codice stampante         *
      *                      *-----------------------------------------*
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-cmd-cst
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      013                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Composizione codice stampante           *
      *                      *-----------------------------------------*
           move      w-sys-spl-cmd-cst
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     def-cll-sys-spc-300.
       def-cll-sys-spc-280.
      *                  *---------------------------------------------*
      *                  * Se file pathname                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero di caratteri ef-  *
      *                      * fettivi per il file pathname            *
      *                      *-----------------------------------------*
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-cmd-pat
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      060                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Composizione file pathname              *
      *                      *-----------------------------------------*
           move      w-sys-spl-cmd-pat
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     def-cll-sys-spc-300.
       def-cll-sys-spc-300.
      *              *-------------------------------------------------*
      *              * Copiatura porzione tra il primo carattere '@'   *
      *              * ed il secondo carattere '@'                     *
      *              *-------------------------------------------------*
       def-cll-sys-spc-320.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su template            *
      *                  *---------------------------------------------*
           add       001                  to   w-sys-spl-inx-tem      .
       def-cll-sys-spc-340.
      *                  *---------------------------------------------*
      *                  * Copiatura                                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-sys-spl-ctr-001      .
           move      spaces               to   w-sys-spl-wkx-060      .
           move      w-sys-spl-inx-tem    to   w-sys-spl-pnt-tem      .
           unstring  w-sys-spl-cmd-tem
                                delimited by   "@"
                                          into w-sys-spl-wkx-060
                                    count in   w-sys-spl-ctr-001
                                  with pointer w-sys-spl-pnt-tem      .
           move      w-sys-spl-wkx-060
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
       def-cll-sys-spc-360.
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
       def-cll-sys-spc-400.
      *              *-------------------------------------------------*
      *              * Composizione della porzione relativa al secondo *
      *              * carattere '@'                                   *
      *              *-------------------------------------------------*
       def-cll-sys-spc-420.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su template            *
      *                  *---------------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-tem      .
           add       001                  to   w-sys-spl-inx-tem      .
       def-cll-sys-spc-440.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo di parame-  *
      *                  * tro                                         *
      *                  *---------------------------------------------*
           if        w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "p" or
                     w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "P"
                     go to def-cll-sys-spc-460
           else if   w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "f" or
                     w-sys-spl-cmd-tex
                    (w-sys-spl-inx-tem)   =    "F"
                     go to def-cll-sys-spc-480
           else      go to def-cll-sys-spc-500.
       def-cll-sys-spc-460.
      *                  *---------------------------------------------*
      *                  * Se codice stampante                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero di caratteri ef-  *
      *                      * fettivi per il codice stampante         *
      *                      *-----------------------------------------*
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-cmd-cst
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      013                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Composizione codice stampante           *
      *                      *-----------------------------------------*
           move      w-sys-spl-cmd-cst
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     def-cll-sys-spc-500.
       def-cll-sys-spc-480.
      *                  *---------------------------------------------*
      *                  * Se file pathname                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero di caratteri ef-  *
      *                      * fettivi per il file pathname            *
      *                      *-----------------------------------------*
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-cmd-pat
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      060                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
      *                      *-----------------------------------------*
      *                      * Composizione file pathname              *
      *                      *-----------------------------------------*
           move      w-sys-spl-cmd-pat
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     def-cll-sys-spc-500.
       def-cll-sys-spc-500.
      *              *-------------------------------------------------*
      *              * Copiatura porzione tra il secondo carattere '@' *
      *              * in poi                                          *
      *              *-------------------------------------------------*
       def-cll-sys-spc-520.
      *                  *---------------------------------------------*
      *                  * Aggiornamento indice su template            *
      *                  *---------------------------------------------*
           add       001                  to   w-sys-spl-inx-tem      .
       def-cll-sys-spc-540.
      *                  *---------------------------------------------*
      *                  * Copiatura                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-sys-spl-wkx-060      .
           unstring  w-sys-spl-cmd-tem    into w-sys-spl-wkx-060
                                  with pointer w-sys-spl-inx-tem      .
           move      zero                 to   w-sys-spl-ctr-002      .
           inspect   w-sys-spl-wkx-060
                                      tallying w-sys-spl-ctr-002
                                          for  trailing  spaces       .
           move      060                  to   w-sys-spl-ctr-001      .
           subtract  w-sys-spl-ctr-002    from w-sys-spl-ctr-001      .
           if        w-sys-spl-ctr-001    =    zero
                     go to def-cll-sys-spc-560.
           move      w-sys-spl-wkx-060
                    (1: w-sys-spl-ctr-001)
                                          to   o-shs
                                              (w-sys-spl-inx-hpr:
                                               w-sys-spl-ctr-001)     .
       def-cll-sys-spc-560.
      *                      *-----------------------------------------*
      *                      * Aggiornamento indice su area o-shs      *
      *                      *-----------------------------------------*
           add       w-sys-spl-ctr-001    to   w-sys-spl-inx-hpr      .
       def-cll-sys-spc-600.
      *              *-------------------------------------------------*
      *              * Copiatura literal fisso per redirezione output  *
      *              * ed errori                                       *
      *              *-------------------------------------------------*
           move      w-sys-spl-cmd-roe    to   o-shs
                                              (w-sys-spl-inx-hpr: )   .
       def-cll-sys-spc-700.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     def-cll-sys-spc-999.
       def-cll-sys-spc-999.
           exit.

      *    *===========================================================*
      *    * Apertura file [stp] in output                             *
      *    *-----------------------------------------------------------*
       opn-out-fil-stp-000.
      *              *-------------------------------------------------*
      *              * Se il flag di errore generale e' in On : nes-   *
      *              * suna azione                                     *
      *              *-------------------------------------------------*
           if        w-exe-job-flg-err    not  = spaces
                     go to opn-out-fil-stp-999.
      *              *-------------------------------------------------*
      *              * Preparazione pathname                           *
      *              *-------------------------------------------------*
           move      w-exe-job-pth-fds    to   f-stp-pat              .
      *              *-------------------------------------------------*
      *              * Open output                                     *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           open      output stp                                       .
      *              *-------------------------------------------------*
      *              * Set dell flag di errore generale                *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to opn-out-fil-stp-999.
       opn-out-fil-stp-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [stp]                                       *
      *    *-----------------------------------------------------------*
       cls-out-fil-stp-000.
      *              *-------------------------------------------------*
      *              * Se il flag di errore generale e' in On : nes-   *
      *              * suna azione                                     *
      *              *-------------------------------------------------*
           if        w-exe-job-flg-err    not  = spaces
                     go to cls-out-fil-stp-999.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     stp                                              .
      *              *-------------------------------------------------*
      *              * Set dell flag di errore generale                *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to cls-out-fil-stp-999.
       cls-out-fil-stp-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [stp]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-fil-stp-000.
      *              *-------------------------------------------------*
      *              * Se il flag di errore generale e' in On : nes-   *
      *              * suna azione                                     *
      *              *-------------------------------------------------*
           if        w-exe-job-flg-err    not  = spaces
                     go to wrt-rec-fil-stp-999.
      *              *-------------------------------------------------*
      *              * Write                                           *
      *              *-------------------------------------------------*
           write     stp-rec                                          .
      *              *-------------------------------------------------*
      *              * Set dell flag di errore generale                *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to wrt-rec-fil-stp-999.
       wrt-rec-fil-stp-999.
           exit.

      *    *===========================================================*
      *    * Append file [eps] a file [stp]                            *
      *    *-----------------------------------------------------------*
       apd-eps-fil-stp-000.
      *              *-------------------------------------------------*
      *              * Se il flag di errore generale e' in On : uscita *
      *              * immediata senza alcuna azione                   *
      *              *-------------------------------------------------*
           if        w-exe-job-flg-err    not  = spaces
                     go to apd-eps-fil-stp-900.
       apd-eps-fil-stp-050.
      *              *-------------------------------------------------*
      *              * Open input file [eps], per lettura in blocchi   *
      *              * da 1024 caratteri                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname                       *
      *                  *---------------------------------------------*
           move      w-trt-fil-eps-pat    to   f-eps-pat              .
      *                  *---------------------------------------------*
      *                  * Open input                                  *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           open      input  eps                                       .
      *                  *---------------------------------------------*
      *                  * Se errore : ad uscita                       *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to apd-eps-fil-stp-900.
       apd-eps-fil-stp-100.
      *              *-------------------------------------------------*
      *              * Close [stp]                                     *
      *              *-------------------------------------------------*
           perform   cls-out-fil-stp-000  thru cls-out-fil-stp-999    .
      *              *-------------------------------------------------*
      *              * Test sul flag di errore generale                *
      *              *-------------------------------------------------*
           if        w-exe-job-flg-err    not  = spaces
                     go to apd-eps-fil-stp-800.
       apd-eps-fil-stp-150.
      *              *-------------------------------------------------*
      *              * Open extend file [st2], che punta a [stp]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname [st2], come [stp]     *
      *                  *---------------------------------------------*
           move      w-exe-job-pth-fds    to   f-st2-pat              .
      *                  *---------------------------------------------*
      *                  * Open extend [st2]                           *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           open      extend st2                                       .
      *                  *---------------------------------------------*
      *                  * Set dell flag di errore generale            *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to apd-eps-fil-stp-800.
       apd-eps-fil-stp-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale binaria file [eps] per ot-  *
      *              * tenere il blocco successivo di 1024 caratteri.  *
      *              *                                                 *
      *              * Viene preventivamente riempita l'area del re-   *
      *              * cord con low-values per poter poi contare il    *
      *              * numero di caratteri effettivamente letti.       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Low values in area record                   *
      *                  *---------------------------------------------*
           move      low-values           to   eps-rec                .
      *                  *---------------------------------------------*
      *                  * Lettura record                              *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      eps   at end
                           go to apd-eps-fil-stp-250.
           if        e-sts                not  = e-not-err
                     go to apd-eps-fil-stp-250.
           go to     apd-eps-fil-stp-300.
       apd-eps-fil-stp-250.
      *              *-------------------------------------------------*
      *              * Se e' stata raggiunta la fine del file [eps],   *
      *              * si chiude il file [st2], e poi si va a riaprire *
      *              * in extend [stp] e a chiudere [eps].             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di errore generale             *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to apd-eps-fil-stp-800.
      *                  *---------------------------------------------*
      *                  * Close [st2]                                 *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     st2                                              .
      *                  *---------------------------------------------*
      *                  * Set dell flag di errore generale            *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to apd-eps-fil-stp-800.
      *                  *---------------------------------------------*
      *                  * Riaggancio                                  *
      *                  *---------------------------------------------*
           go to     apd-eps-fil-stp-800.
       apd-eps-fil-stp-300.
      *              *-------------------------------------------------*
      *              * Determinazione del numero di trailing low-value *
      *              * e di conseguenza del numero di caratteri effet- *
      *              * tivamente letti                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-trt-fil-eps-tlw      .
           inspect   eps-rec          tallying w-trt-fil-eps-tlw
                                  for trailing low-values             .
           move      1024                 to   w-trt-fil-eps-eff      .
           subtract  w-trt-fil-eps-tlw    from w-trt-fil-eps-eff      .
       apd-eps-fil-stp-350.
      *              *-------------------------------------------------*
      *              * Se sono stati letti zero caratteri effettivi :  *
      *              * come se fosse stata raggiunta la fine del file  *
      *              *-------------------------------------------------*
           if        w-trt-fil-eps-eff    =    zero
                     go to apd-eps-fil-stp-250.
      *              *-------------------------------------------------*
      *              * Se non e' stato letto un record intero si va a  *
      *              * completare la scrittura del file [stp] con      *
      *              * blocchi di 1 carattere per volta                *
      *              *-------------------------------------------------*
           if        w-trt-fil-eps-eff    <    1024
                     go to apd-eps-fil-stp-500.
       apd-eps-fil-stp-400.
      *              *-------------------------------------------------*
      *              * Se e' stato letto un record intero da 1024 ca-  *
      *              * ratteri lo si ricopia nel file [stp] mediante   *
      *              * il file [st2]                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura                                   *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           move      eps-rec              to   st2-rec                .
           write     st2-rec                                          .
      *                  *---------------------------------------------*
      *                  * Set dell flag di errore generale            *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to apd-eps-fil-stp-250.
       apd-eps-fil-stp-450.
      *              *-------------------------------------------------*
      *              * Quindi si ricicla a leggere il successivo bloc- *
      *              * co da 1024 caratteri dal file [eps]             *
      *              *-------------------------------------------------*
           go to     apd-eps-fil-stp-200.
       apd-eps-fil-stp-500.
      *              *-------------------------------------------------*
      *              * Close file [st2]                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     st2                                              .
      *                  *---------------------------------------------*
      *                  * Set dell flag di errore generale            *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to apd-eps-fil-stp-800.
       apd-eps-fil-stp-550.
      *              *-------------------------------------------------*
      *              * Open extend file [st2], che punta a [stp], per  *
      *              * eseguire la copiatura in blocchi da 1 carattere *
      *              * per volta, per i caratteri finali               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname [st3], come [stp]     *
      *                  *---------------------------------------------*
           move      w-exe-job-pth-fds    to   f-st3-pat              .
      *                  *---------------------------------------------*
      *                  * Open extend [st3]                           *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           open      extend st3                                       .
      *                  *---------------------------------------------*
      *                  * Set dell flag di errore generale            *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to apd-eps-fil-stp-800.
       apd-eps-fil-stp-600.
      *              *-------------------------------------------------*
      *              * Copiatura dei caratteri finali                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-trt-fil-eps-inx      .
       apd-eps-fil-stp-650.
           add       1                    to   w-trt-fil-eps-inx      .
           if        w-trt-fil-eps-inx    >    w-trt-fil-eps-eff
                     go to apd-eps-fil-stp-700.
           move      eps-chr
                    (w-trt-fil-eps-inx)   to   st3-chr                .
           move      e-not-err            to   e-sts                  .
           write     st3-rec                                          .
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to apd-eps-fil-stp-800.
           go to     apd-eps-fil-stp-650.
       apd-eps-fil-stp-700.
      *              *-------------------------------------------------*
      *              * Close file [st3]                                *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     st3                                              .
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to apd-eps-fil-stp-800.
       apd-eps-fil-stp-800.
      *              *-------------------------------------------------*
      *              * Open extend [stp]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di errore generale             *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to apd-eps-fil-stp-850.
      *                  *---------------------------------------------*
      *                  * Preparazione pathname                       *
      *                  *---------------------------------------------*
           move      w-exe-job-pth-fds    to   f-stp-pat              .
      *                  *---------------------------------------------*
      *                  * Open extend                                 *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           open      extend stp                                       .
      *                  *---------------------------------------------*
      *                  * Set dell flag di errore generale            *
      *                  *---------------------------------------------*
           if        e-sts                not  = e-not-err
                     move  "#"            to   w-exe-job-flg-err
                     go to apd-eps-fil-stp-850.
       apd-eps-fil-stp-850.
      *              *-------------------------------------------------*
      *              * Close file [eps]                                *
      *              *-------------------------------------------------*
           close     eps                                              .
       apd-eps-fil-stp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     apd-eps-fil-stp-999.
       apd-eps-fil-stp-999.
           exit.
