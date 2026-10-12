/*    *==========================================================*/
/*    * Tabella [zci] Codici IVA                                 */
/*    *                                                          */
/*    * Aggiornamento 08.10.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_iva),
/*       *=======================================================*/
/*       * Codice Iva                                            */
/*       *                                                       */
/*       * Legenda Codici Iva                                    */
/*       * ------------------                                    */
/*       *                                                       */
/*       * da   1 a  99 - % imponibili                           */
/*       * da 201 a 299 - (libero)                               */
/*       * da 301 a 399 - Non imponibili                         */
/*       * da 401 a 499 - Esenti                                 */
/*       * da 501 a 599 - Esclusi                                */
/*       * da 601 a 698 - Fuori campo iva                        */
/*       *          699 - Aliquota zero                          */
/*       * da 801 a 899 - (libero)                               */
/*       *                                                       */
/*       * I seguenti codici Iva non esistono in tabella ma      */
/*       * possono essere utilizzati                             */
/*       *                                                       */
/*       * 1xx = scorporo per aliquota xx             (Obsoleti) */
/*       * 7xx = non detraibile per aliquota xx                  */
/*       * 9xx = sola IVA senza imponibile per xx                */
/*       *                                                       */
/*       * CONVENZIONE IMPORTANTE                                */
/*       * ======================                                */
/*       *                                                       */
/*       * Per introdurre la gestione dei Codici Iva con i deci- */
/*       * mali, si e' scelto che gli eventuali decimali (massi- */
/*       * mo 2) vengano posizionati nei primi due caratteri del */
/*       * codice.                                               */
/*       *                                                       */
/*       * Alcuni esempi:                                        */
/*       *                                                       */
/*       * Aliquota 11,5%  - codice 50011 - 1 decimale           */
/*       * Aliquota 23,4%  - codice 40023 - 1 decimale           */
/*       * Aliquota 23,45% - codice 45023 - 2 decimali           */
/*       *                                                       */
/*       * Aliquota 23,4% - non detraibile - 40723               */
/*       * Aliquota 23,4% - omaggio        - 40923               */
/*       *                                                       */
/*       * Questo si e' reso necessario per evitare di confonde- */
/*       * re le aliquote con decimali con i codici di esenzione */
/*       * Iva (>100 900<)                                       */
/*       *-------------------------------------------------------*/
  cod_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Mnemonico                                             */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per i documenti                           */
/*       *-------------------------------------------------------*/
  des_doc                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per la legenda                            */
/*       *-------------------------------------------------------*/
  des_leg                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione interna                                   */
/*       *-------------------------------------------------------*/
  des_int                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Commento                                              */
/*       *-------------------------------------------------------*/
  com_int                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Decimali aliquota                                     */
/*       *-------------------------------------------------------*/
  dec_ali                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Aliquota IVA                                          */
/*       *-------------------------------------------------------*/
  ali_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no va in liquidazione periodica                    */
/*       *                                                       */
/*       * - 1 : Si                                              */
/*       * - 0 : No                                              */
/*       *-------------------------------------------------------*/
  snx_lqp                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no bollo virtuale                                  */
/*       *                                                       */
/*       * - 1 : Si                                              */
/*       * - 0 : No                                              */
/*       *-------------------------------------------------------*/
  snx_blv                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice Natura dell'operazione in fatture elettroniche */
/*       *                                                       */
/*       * (Fattura PA - SDI - Agg. 04/05/2020)                  */
/*       *                                                       */
/*       * ----------------------------------------------------- */
/*       *                                                       */
/*       * 'N1  ' = escluse ex art. 15	                       */
/*       *                                                       */
/*       * 'N2  ' = non soggette	                              */
/*       * 'N2.1' = non soggette ad IVA ai sensi degli artt.     */
/*       *          da 7 a 7-septies del DPR 633/72              */
/*       * 'N2.2' = non soggette - altri casi                    */
/*       *                                                       */
/*       * 'N3  ' = non imponibili	                              */
/*       * 'N3.1' = non imponibili - esportazioni                */
/*       * 'N3.2' = non imponibili - cessioni intracomunitarie   */
/*       * 'N3.3' = non imponibili - cessioni verso San Marino   */
/*       * 'N3.4' = non imponibili - operazioni assimilate       */
/*       *          alle cessioni all'esportazione               */
/*       * 'N3.5' = non imponibili - a seguito di dichiarazioni  */
/*       *          d'intento                                    */
/*       * 'N3.6' = non imponibili - altre operazioni che non    */
/*       *          concorrono alla formazione del plafond       */
/*       *                                                       */
/*       * 'N4  ' = esenti                                       */
/*       *                                                       */
/*       * 'N5  ' = regime del margine / IVA non esposta in fat- */
/*       *          tura                                         */
/*       *                                                       */
/*       * 'N6  ' = inversione contabile (per le operazioni in   */
/*       *          reverse charge ovvero nei casi di autofat-   */
/*       *          turazione per acquisti extra UE di servizi   */
/*       *          ovvero per importazioni di beni nei soli     */
/*       *          casi previsti)				         */
/*       * 'N6.1' = inversione contabile - cessione di rottami   */
/*       *           e altri materiali di recupero               */
/*       * 'N6.2' = inversione contabile - cessione di oro e     */
/*       *          argento puro	 		                */
/*       * 'N6.3' = inversione contabile - subappalto nel  	  */
/*       *          settore edile	 		                */
/*       * 'N6.4' = inversione contabile - cessione fabbricati   */
/*       * 'N6.5' = inversione contabile - cessione di telefoni  */
/*       *          cellulari	 		                */
/*       * 'N6.6' = inversione contabile - cessione di prodotti  */
/*       *          elettronici	 		                */
/*       * 'N6.7' = inversione contabile - prestazioni comparto  */
/*       *          edile e settori connessi	 	         */
/*       * 'N6.8' = inversione contabile - operazioni settore    */
/*       *          energetico	 		                */
/*       * 'N6.9' = inversione contabile - altri casi            */
/*       *                                                       */
/*       * 'N7  ' = IVA assolta in altro stato UE (vendite a     */
/*       *          distanza ex art. 40 c. 3 e 4 e art. 41 c. 1  */
/*       *          lett. b,  DL 331/93; prestazione di servizi  */
/*       *          di telecomunicazioni, tele-radiodiffusione   */
/*       *          ed elettronici ex art. 7-sexies lett. f, g,  */
/*       *          art. 74-sexies DPR 633/72)	 		  */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  cod_nop                   varchar (5)                 not null
