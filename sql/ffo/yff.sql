/*    *==========================================================*/
/*    * Tabella [yff] Tabella tipi movimento               {ffo} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_tmo),
/*       *=======================================================*/
/*       * Codice tipo movimento                                 */
/*       *-------------------------------------------------------*/
  cod_tmo                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per uso interno uppercase                 */
/*       *-------------------------------------------------------*/
  des_key                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione per uso interno                           */
/*       *-------------------------------------------------------*/
  des_tmo                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Password per il tipo movimento                        */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  pwd_tmo                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento associato al tipo movimento            */
/*       *                                                       */
/*       * - 01 : Fattura                                        */
/*       * - 02 : Nota di addebito                               */
/*       * - 03 : Nota di accredito                              */
/*       *-------------------------------------------------------*/
  tip_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo provenienza del documento                        */
/*       *                                                       */
/*       * - 01 : Nazionale                                      */
/*       * - 02 : CEE                                            */
/*       * - 03 : Estero                                         */
/*       * - 04 : Dogana                                         */
/*       *-------------------------------------------------------*/
  prv_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No impostazione righe del documento                */
/*       *-------------------------------------------------------*/
  snx_ird                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di giornale iva per il documento associato al  */
/*       * tipo movimento                                        */
/*       *-------------------------------------------------------*/
  num_giv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per movimento di contabilita'          */
/*       *                                                       */
/*       * N.B. : Se a zero non si esegue movimentazione di      */
/*       *        contabilita' a fronte di questo documento      */
/*       *-------------------------------------------------------*/
  cau_cge                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale per movimento di magazzino             */
/*       *                                                       */
/*       * N.B. : Se a zero non si esegue movimentazione di      */
/*       *        magazzino a fronte di questo documento         */
/*       *-------------------------------------------------------*/
  cau_mag                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Movimento a fronte di :                               */
/*       *                                                       */
/*       * - 01 : Niente                                         */
/*       * - 02 : Bolla di consegna fornitore                    */
/*       *-------------------------------------------------------*/
  mov_afd                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento di default se movimento a fronte       */
/*       *                                                       */
/*       * N.B.: Significativo solo se campo precedente diverso  */
/*       *       da zero                                         */
/*       *-------------------------------------------------------*/
  def_tmf                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Validita' per le dipendenze                           */
/*       *                                                       */
/*       *  - spaces : per tutte le dipendenze                   */
/*       *  - S      : solo per la Sede                          */
/*       *  - D      : solo per le dipendenze                    */
/*       *  - X      : solo per le dipendenze                    */
/*       *             indicate                                  */
/*       *-------------------------------------------------------*/
  vld_dpz                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Default accettazione tipo riga corpo                  */
/*       *-------------------------------------------------------*/
  def_tpr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione                                   */
/*       *-------------------------------------------------------*/
  cod_dsl                   varchar (07)                not null,
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
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
