/*    *==========================================================*/
/*    * Tabella [sfp] Pagamenti scadenze fornitori         {scf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_pgf),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_pgf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di pagamento scadenza    */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di pagamento                            */
/*       *-------------------------------------------------------*/
  drc_pgf                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di pagamento                            */
/*       *-------------------------------------------------------*/
  npc_pgf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di pagamento                                */
/*       *                                                       */
/*       * - 01 : Per Contanti                                   */
/*       * - 02 : Con Assegno                                    */
/*       * - 03 : Contro Addebito in c/c Bancario                */
/*       * - 04 : Per C/C postale                                */
/*       * - 13 : Ordine di Bonifico Bancario                    */
/*       * - 14 : Bollettino in c/c postale                      */
/*       * - 15 : Incarico a Banca su Avviso di Scadenza         */
/*       *-------------------------------------------------------*/
  mod_pgf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di pagamento richiesto                           */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Contro addebito nel c/c bancario o postale     */
/*       * - 02 : Contro versamento in cassa                     */
/*       *                                                       */
/*       * N.B.: Significativo solo in caso di modalita' di pa-  */
/*       *       gamento pari a :                                */
/*       *                                                       */
/*       *       - 13 : Ordine di Bonifico Bancario              */
/*       *       - 14 : Bollettino in c/c postale                */
/*       *       - 15 : Incarico a Banca su Avviso di Scadenza   */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  tip_pgf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo valuta per il pagamento                          */
/*       *                                                       */
/*       * - 01 : Pagamento in valuta base                       */
/*       * - 02 : Pagamento in valuta diversa da quella base     */
/*       *-------------------------------------------------------*/
  tvl_pgf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice fornitore                                      */
/*       *-------------------------------------------------------*/
  cod_fnt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza del fornitore                       */
/*       *-------------------------------------------------------*/
  dpz_fnt                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamento al fornitore                        */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla dipendenza                                */
/*       *-------------------------------------------------------*/
  inl_pgf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra cassa per pagamento in caso di modali-  */
/*       * ta' di pagamento pari a :                             */
/*       *                                                       */
/*       * - 01 : Per Contanti                                   */
/*       *                                                       */
/*       * oppure                                                */
/*       *                                                       */
/*       * Codice nostra banca per pagamento in caso di modali-  */
/*       * ta' di pagamento pari a :                             */
/*       *                                                       */
/*       * - 02 : Con Assegno                                    */
/*       * - 03 : Contro Addebito in c/c Bancario                */
/*       * - 13 : Ordine di Bonifico Bancario                    */
/*       * - 14 : Bollettino in c/c postale                      */
/*       * - 15 : Incarico a Banca su Avviso di Scadenza         */
/*       *                                                       */
/*       * oppure                                                */
/*       *                                                       */
/*       * Codice nostro c/c postale per pagamento in caso di    */
/*       * modalita' di pagamento pari a :                       */
/*       *                                                       */
/*       * - 04 : Per C/C postale                                */
/*       * - 14 : Bollettino in c/c postale                      */
/*       *-------------------------------------------------------*/
  cbp_pgf                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI banca fornitore per pagamento in caso di   */
/*       * modalita' di pagamento pari a :                       */
/*       *                                                       */
/*       * - 13 : Ordine di Bonifico Bancario                    */
/*       *                                                       */
/*       * e tipo valuta per il pagamento e' :                   */
/*       *                                                       */
/*       * - 01 : Pagamento in valuta base                       */
/*       *-------------------------------------------------------*/
  abi_pgf                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CAB banca fornitore per pagamento in caso di   */
/*       * modalita' di pagamento pari a :                       */
/*       *                                                       */
/*       * - 13 : Ordine di Bonifico Bancario                    */
/*       *                                                       */
/*       * e tipo valuta per il pagamento e' :                   */
/*       *                                                       */
/*       * - 01 : Pagamento in valuta base                       */
/*       *-------------------------------------------------------*/
  cab_pgf                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice banca estera fornitore per pagamento in caso   */
/*       * di modalita' di pagamento pari a :                    */
/*       *                                                       */
/*       * - 13 : Ordine di Bonifico Bancario                    */
/*       *                                                       */
/*       * e tipo valuta per il pagamento e' :                   */
/*       *                                                       */
/*       * - 02 : Pagamento in valuta diversa da quella base     */
/*       *-------------------------------------------------------*/
  bef_pgf                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto corrente bancario fornitore per pagamento in    */
/*       * caso di modalita' di pagamento pari a :               */
/*       *                                                       */
/*       * - 13 : Ordine di Bonifico Bancario                    */
/*       *                                                       */
/*       * e tipo valuta per il pagamento e' :                   */
/*       *                                                       */
/*       * - 01 : Pagamento in valuta base                       */
/*       *                                                       */
/*       * oppure                                                */
/*       *                                                       */
/*       * Conto corrente postale fornitore per pagamento in ca- */
/*       * so di modalita' di pagamento pari a :                 */
/*       *                                                       */
/*       * - 14 : Bollettino in c/c postale                      */
/*       *-------------------------------------------------------*/
  ccc_pgf                   varchar (20)                not null,
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
/*       * Importo pagamento, in valuta                          */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito, in quanto i pa-  */
/*       *       menti di scadenze con valuta diversa da quella  */
/*       *       base vengono attualmente gestiti con il tipo    */
/*       *       operazione 'Richiesta di Pagamento su Estero',  */
/*       *       che non prevede in questa fase alcuna registra- */
/*       *       zione contabile                                 */
/*       *-------------------------------------------------------*/
  iiv_pgf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo pagamento                                     */
/*       *                                                       */
/*       * N.B. : Relativo sia a scadenze in valuta base, che    */
/*       *        non.                                           */
/*       *                                                       */
/*       * N.B. : Rappresenta la somma degli importi scadenze    */
/*       *        coinvolti nel pagamento, eventualmente modifi- */
/*       *        cato per sconti o abbuoni.                     */
/*       *-------------------------------------------------------*/
  imp_pgf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese addebitate dalla banca per il pagamento */
/*       * in caso di modalita' di pagamento pari a :            */
/*       * - 03 : Contro Addebito in c/c Bancario                */
/*       *                                                       */
/*       * oppure                                                */
/*       *                                                       */
/*       * Importo spese addebitate dalla Posta per il pagamento */
/*       * in caso di modalita' di pagamento pari a :            */
/*       * - 04 : Per C/C postale                                */
/*       *                                                       */
/*       * oppure                                                */
/*       *                                                       */
/*       * Importo spese sostenute in caso di pagamento per con- */
/*       * tanti presso banca o posta                            */
/*       *-------------------------------------------------------*/
  spe_pgf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento per il pagamento                       */
/*       *                                                       */
/*       * Solamente per modalita' di pagamento pari a :         */
/*       * - 01 : Per Contanti                                   */
/*       * - 02 : Con Assegno                                    */
/*       * - 03 : Contro Addebito in c/c Bancario                */
/*       * - 04 : Per C/C postale                                */
/*       *-------------------------------------------------------*/
  ddo_pgf                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento per il pagamento                     */
/*       *                                                       */
/*       * Solamente per modalita' di pagamento pari a :         */
/*       * - 01 : Per Contanti                                   */
/*       * - 02 : Con Assegno                                    */
/*       * - 03 : Contro Addebito in c/c Bancario                */
/*       * - 04 : Per C/C postale                                */
/*       *-------------------------------------------------------*/
  ndo_pgf                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Data valuta fissa per il beneficiario                 */
/*       *                                                       */
/*       * Solamente per modalita' di pagamento pari a :         */
/*       * - 13 : Ordine di Bonifico Bancario                    */
/*       *-------------------------------------------------------*/
  dvf_bfc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Segnale acconto/saldo per il pagamento                */
/*       * - 00 : non significativo, ovvero a saldo              */
/*       * - 01 : a  saldo                                       */
/*       * - 02 : in conto                                       */
/*       *                                                       */
/*       * N.B. : Questo segnale ha valore globale, in quanto e' */
/*       *        relativo a tutte le scadenze coinvolte nel pa- */
/*       *        gamento.                                       */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  aos_pgf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di si/no registrazione nuova scadenza per il  */
/*       * residuo da pagare, solo se il segnale acconto/saldo   */
/*       * per il pagamento indica 'in conto'                    */
/*       * - 01 : Si                                             */
/*       * - 02 : No                                             */
/*       *-------------------------------------------------------*/
  rns_pgf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero nuova scadenza per la differenza tra l'importo */
/*       * della scadenza e l'importo pagato : s.aa.nnnnnnnn     */
/*       *-------------------------------------------------------*/
  nns_pgf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo nuova scadenza a fronte del movimento di paga-   */
/*       * mento                                                 */
/*       *                                                       */
/*       * N.B.: In caso di 'Richiesta di pagamento su Estero'   */
/*       *       questo campo viene forzato al valore 07         */
/*       *-------------------------------------------------------*/
  tns_pgf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero operazione di addebito, se scadenza addebita-  */
/*       * ta, nel formato : s.aa.nnnnnnnn                       */
/*       *                                                       */
/*       * Solamente per modalita' di pagamento pari a :         */
/*       *                                                       */
/*       * - 13 : Ordine di Bonifico Bancario                    */
/*       * - 14 : Bollettino in c/c postale                      */
/*       * - 15 : Incarico a Banca su Avviso di Scadenza         */
/*       *-------------------------------------------------------*/
  num_adp                   bigint  (11)                not null,
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

