/*    *==========================================================*/
/*    * Tabella [zfi] Tipi movimento fatture di vendita    {fat} */
/*    *                                                          */
/*    * Aggiornamento 03.10.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (cod_tmo),
/*       *=======================================================*/
/*       * Codice tipo movimento                                 */
/*       *-------------------------------------------------------*/
  cod_tmo                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo movimento per uso interno            */
/*       *-------------------------------------------------------*/
  des_tmo                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Password per il tipo movimento                        */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  pwd_tmo                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Validita' per le dipendenze                           */
/*       *                                                       */
/*       * - 01 : Documento valido per tutte le dipendenze       */
/*       * - 02 : Documento valido per una sola dipendenza       */
/*       *-------------------------------------------------------*/
  vld_dpz                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza per cui il documento e' valido      */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento associato al tipo movimento            */
/*       *                                                       */
/*       * - 01 : Fattura                                        */
/*       * - 02 : Nota di addebito                               */
/*       * - 03 : Nota di accredito                              */
/*       * - 04 : Fattura Pro-Forma                              */
/*       *-------------------------------------------------------*/
  tip_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo origine del documento associato a tipo movimento */
/*       *                                                       */
/*       * - Se tipo documento associato 01                      */
/*       *                                                       */
/*       *   - 01 : Manuale, fattura o nota credito o debito     */
/*       *   - 02 : Manuale, fattura + bolla di consegna         */
/*       *   - 03 : Manuale, fattura + ricevuta fiscale          */
/*       *   - 11 : Automatica, a fronte bolla di uscita         */
/*       *                                                       */
/*       * - Se tipo documento associato 03                      */
/*       *                                                       */
/*       *   - 31 : Automatica, a fronte bolla di entrata        */
/*       *                                                       */
/*       * - Se tipo documento associato 04                      */
/*       *                                                       */
/*       *   - 41 : Automatica, a fronte ordine                  */
/*       *-------------------------------------------------------*/
  org_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo provenienza del documento se origine automatica  */
/*       *                                                       */
/*       * - Se tipo documento associato 01 o 03                 */
/*       *   - Se tipo origine del documento 11                  */
/*       *     - 01 : Fatturazione automatica da bolle di conse- */
/*       *            gna                                        */
/*       *-------------------------------------------------------*/
  prv_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla numerazione per il documento associato al tipo  */
/*       * movimento                                             */
/*       *-------------------------------------------------------*/
  sgl_num                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di giornale iva per il documento associato al  */
/*       * tipo movimento                                        */
/*       *-------------------------------------------------------*/
  num_giv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo documento associato al tipo movimen- */
/*       * to per la stampa                                      */
/*       *-------------------------------------------------------*/
  des_stp                   varchar (30)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per movimento di contabilita'          */
/*       *                                                       */
/*       * N.B. : Se a zero non si esegue movimentazione di      */
/*       *        contabilita' a fronte di questo documento      */
/*       *-------------------------------------------------------*/
  cau_cge                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice sottoconto iva vendite per il movimento di     */
/*       * contabilita' generale relativo al documento           */
/*       *                                                       */
/*       * N.B. : Se questo valore e' diverso da zero, esso pre- */
/*       *        vale sulla personalizzazione del sottoconto    */
/*       *        iva vendite di contabilita'                    */
/*       *-------------------------------------------------------*/
  ctp_ivv                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita vendite per il movimento di con- */
/*       * tabilita' generale relativo al documento              */
/*       *                                                       */
/*       * N.B. : Se questo valore e' diverso da zero, esso pre- */
/*       *        vale sugli altri nell'algoritmo di determina-  */
/*       *        zione della contropartita per la riga di ven-  */
/*       *        dita.                                          */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Default per accettazione tipo riga corpo              */
/*       *-------------------------------------------------------*/
  def_tpr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa riferimento all'Agente                   */
/*       *                                                       */
/*       *  - 'S' : Codice e Nominativo dell'Agente              */
/*       *  - 'N' : Nessun riferimento all'Agente                */
/*       *  - 'C' : Solo il codice dell'Agente                   */
/*       *  - 'T' : Solo il nominativo dell'Agente               */
/*       *-------------------------------------------------------*/
  snx_age                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa riferimento alla conferma ordine         */
/*       *                                                       */
/*       *  - ' ' o 'S' : Si riferimento alla conferma ordine,   */
/*       *                se documento in relazione              */
/*       *  - 'N'       : Nessun riferimento alla conferma       */
/*       *-------------------------------------------------------*/
  snx_rco                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No stampa riferimento al documento di trasporto    */
/*       *                                                       */
/*       *  - ' ' o 'S' : Si riferimento al documento di tras-   */
/*       *                porto, se documento in relazione       */
/*       *  - 'N'       : Nessun riferimento alla conferma       */
/*       *-------------------------------------------------------*/
  snx_rdt                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dicitura da stampare a fine righe              */
/*       *-------------------------------------------------------*/
  cod_dct                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Default tipo vendita per l'agente                     */
/*       *-------------------------------------------------------*/
  dtp_vpa                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Default modulo da stampare                            */
/*       *-------------------------------------------------------*/
  def_mdf                   varchar (08)                not null,
/*       *-------------------------------------------------------*/
/*       * Status per il tipo movimento                          */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sta_tus                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di determinazione dello status                   */
/*       *                                                       */
/*       * Solo se status diverso da 01                          */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sta_tud                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo movimento di riferimento                  */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sta_tuc                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di trattamento nelle statistiche per i      */
/*       * documenti precedenti il cambiamento di status         */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sta_tux                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo documento per fatturazione elettronica    */
/*       *                                                       */
/*       * - 'TD01' : fattura				         */
/*       * - 'TD02' : acconto/anticipo su fattura		  */							
/*       * - 'TD03' : acconto/anticipo su parcella	         */
/*       * - 'TD04' : nota di credito	                       */
/*       * - 'TD05' : nota di debito		                */
/*       * - 'TD06' : parcella                                   */
/*       *                                                       */
/*       * - 'TD07' :  Fattura semplificata                      */
/*       * - 'TD08' :  Nota di Credito semplificata              */
/*       * - 'TD10' :  Fattura per acquisto intracomunitario beni*/
/*       * - 'TD11' :  Fattura per acquisto intracomunitario ser.*/
/*       * - 'TD12' :  Documento riepilogativo (art.6, c. 1 ...) */
/*       *                                                       */
/*       * - 'TD16' : integrazione fattura reverse charge int.   */
/*       * - 'TD17' : integrazione/autofattura per acquisto      */
/*       *            servizi dall'estero                        */
/*       * - 'TD18' : integrazione acquisto beni intracomunitari */		
/*       * - 'TD19' : integrazione/autofattura per acquisto di   */
/*       *            beni ex art.17 c.2 DPR 633/72              */
/*       * - 'TD20' : autofattura per regolarizzazione e inte-	  */
/*       *            grazione delle fatture (ex art.6 c.8       */
/*       *            d.lgs. 471/97  o  art.46 c.5 D.L. 331/93)  */
/*       * - 'TD21' : autofattura per splafonamento              */
/*       * - 'TD22' : estrazione beni da Deposito IVA            */
/*       * - 'TD23' : estrazione beni da Deposito IVA con        */
/*       *            versamento dell'IVA                        */
/*       * - 'TD24' : fattura differita di cui all'art. 21,      */
/*       *            comma 4, lett. a)                          */
/*       * - 'TD25' : fattura differita di cui all'art. 21,      */
/*       *            comma 4, terzo periodo lett. b)            */
/*       * - 'TD26' : cessione di beni ammortizzabili e per      */
/*       *            passaggi interni (ex art.36 DPR 633/72)    */
/*       * - 'TD27' : fattura per autoconsumo o per cessioni     */
/*       *            gratuite senza rivalsa                     */
/*       *-------------------------------------------------------*/
  tdo_fel                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice modulo di default per la stampa                */
/*       *-------------------------------------------------------*/
  cod_mod                   varchar (08)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
  