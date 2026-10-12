/*    *==========================================================*/
/*    * Tabella [sfa] Addebiti pagamenti scadenze          {scf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_adp),
/*       *=======================================================*/
/*       * Numero addebito : s.aa.nnnnnnnn                       */
/*       *-------------------------------------------------------*/
  num_adp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di addebito              */
/*       *-------------------------------------------------------*/
  dtr_adp                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di addebito pagamento                   */
/*       *-------------------------------------------------------*/
  drc_adp                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di addebito pagamento                   */
/*       *-------------------------------------------------------*/
  npc_adp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di addebito                                 */
/*       *                                                       */
/*       * - 13 : Di bonifico bancario                           */
/*       * - 14 : Di C/C postale                                 */
/*       * - 15 : Di incarico a Banca su Avviso di Scadenza      */
/*       *-------------------------------------------------------*/
  mod_adp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di pagamento eseguito                            */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Contro addebito nel c/c bancario o postale     */
/*       * - 02 : Contro versamento in cassa                     */
/*       *                                                       */
/*       * N.B.: Significativo solo in caso di modalita' di pa-  */
/*       *       gamento pari a :                                */
/*       *                                                       */
/*       *       - 13 : Bonifico bancario                        */
/*       *       - 14 : C/C postale                              */
/*       *       - 15 : Incarico a Banca su Avviso di Scadenza   */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  tip_adp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo valuta per l'addebito                            */
/*       *                                                       */
/*       * - 01 : Addebito in valuta base                        */
/*       * - 02 : Addebito in valuta diversa da quella base      */
/*       *-------------------------------------------------------*/
  tvl_adp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca per pagamento in caso di modali-  */
/*       * ta' di addebito pari a :                              */
/*       *                                                       */
/*       * - 13 : Bonifico bancario                              */
/*       * - 15 : Incarico a Banca su Avviso di Scadenza         */
/*       *                                                       */
/*       * oppure                                                */
/*       *                                                       */
/*       * Codice nostro c/c postale per pagamento in caso di    */
/*       * modalita' di addebito pari a :                        */
/*       *                                                       */
/*       * - 14 : C/C postale                                    */
/*       *-------------------------------------------------------*/
  cbp_adp                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra cassa per pagamento in caso di modali-  */
/*       * ta' di addebito pari a :                              */
/*       *                                                       */
/*       * - 13 : Bonifico bancario                              */
/*       * - 14 : C/C postale                                    */
/*       * - 15 : Incarico a Banca su Avviso di Scadenza         */
/*       *                                                       */
/*       *   purche' il tipo pagamento sia :                     */
/*       *                                                       */
/*       * - 02 : Contro versamento in cassa                     */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  cas_adp                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per il pagamento                               */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vpf                   varchar (03)                not null,
  dec_vpf                   tinyint (01)                not null,
  tdc_vpf                   varchar (01)                not null,
  cdc_vpf                   decimal (12,05)             not null,
/*       *-------------------------------------------------------*/
/*       * Importo addebito, in valuta                           */
/*       *                                                       */
/*       * N.B. : Solamente se relativo a pagamenti in valuta    */
/*       *        diversa dalla valuta base.                     */
/*       *                                                       */
/*       * N.B. : Rappresenta la somma degli importi pagamenti   */
/*       *        in valuta coinvolti nell'addebito, non modi-   */
/*       *        ficabile.                                      */
/*       *-------------------------------------------------------*/
  iiv_adp                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo addebito                                      */
/*       *                                                       */
/*       * N.B. : Se relativo a pagamenti in valuta diversa dal- */
/*       *        la valuta base, rappresenta il controvalore in */
/*       *        valuta base degli importi pagamenti in valuta  */
/*       *        coinvolti nell'addebito, cosi' come comunicato */
/*       *        dalla banca. Da questo si puo' dedurre il cam- */
/*       *        bio applicato.                                 */
/*       *                                                       */
/*       * N.B. : Se relativo a pagamenti in valuta base, rap-   */
/*       *        presenta la somma degli importi pagamenti in   */
/*       *        valuta base coinvolti nell'addebito. Non puo'  */
/*       *        essere modificato.                             */
/*       *-------------------------------------------------------*/
  imp_adp                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese addebitate dalla banca per l'addebito   */
/*       * in caso di modalita' di addebito pari a :             */
/*       *                                                       */
/*       * - 13 : Bonifico bancario                              */
/*       * - 15 : Incarico a Banca su Avviso di Scadenza         */
/*       *                                                       */
/*       * oppure                                                */
/*       *                                                       */
/*       * Importo spese addebitate dalla Posta per l'addebito   */
/*       * in caso di modalita' di addebito pari a :             */
/*       * - 14 : C/C postale                                    */
/*       *-------------------------------------------------------*/
  spe_adp                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento per l'addebito                         */
/*       *-------------------------------------------------------*/
  ddo_adp                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento per l'addebito                       */
/*       *-------------------------------------------------------*/
  ndo_adp                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags bloccanti                                       */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       * 06 : (libero)                                         */
/*       * 07 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_bl1                   varchar (01)                not null,
  flg_bl2                   varchar (01)                not null,
  flg_bl3                   varchar (01)                not null,
  flg_bl4                   varchar (01)                not null,
  flg_bl5                   varchar (01)                not null,
  flg_bl6                   varchar (01)                not null,
  flg_bl7                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags non bloccanti                                   */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   varchar (01)                not null,
  flg_nb2                   varchar (01)                not null,
  flg_nb3                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia effettuabile                         */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

