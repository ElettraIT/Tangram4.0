/*    *==========================================================*/
/*    * Tabella [sfs] Scadenze fornitori                   {scf} */
/*    *                                                          */
/*    * Aggiornamento 08.08.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (num_scf),
/* */
  KEY                       NUMPGF                      (num_pgf),
/* */
  KEY                       CODFNT                      (cod_fnt),
/* */
  KEY                       DATSCA                      (dat_sca),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_scf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di emissione scadenza    */
/*       * fornitore                                             */
/*       *-------------------------------------------------------*/
  dtr_rgs                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di registrazione o cessione a fornitore */
/*       *-------------------------------------------------------*/
  drc_rgs                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di registrazione o cessione a fornitore */
/*       *-------------------------------------------------------*/
  npc_rgs                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di fatture fornitori      */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  prt_ffo                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo scadenza                                         */
/*       * - 01 : Rimessa diretta                                */
/*       * - 02 : Incasso elettronico                            */
/*       * - 03 : Ri.Ba.                                         */
/*       * - 04 : C.d.O.                                         */
/*       * - 05 : M.Av.                                          */
/*       * - 06 : R.I.D.                                         */
/*       * - 07 : Bonifico bancario                              */
/*       * - 08 : C/C postale                                    */
/*       * - 09 : Ricevuta bancaria                              */
/*       * - 10 : Tratta                                         */
/*       * - 11 : Paghero' cambiario                             */
/*       *-------------------------------------------------------*/
  tip_scf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo registrazione scadenza                           */
/*       * - 01 : Registrazione interna                          */
/*       * - 02 : Cessione a fornitore                           */
/*       *-------------------------------------------------------*/
  trs_scf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo valuta per la scadenza                           */
/*       * - 01 : Scadenza in valuta base                        */
/*       * - 02 : Scadenza in valuta diversa da quella base      */
/*       *                                                       */
/*       * N.B.: nuova definizione (01/10/01)                    */
/*       *                                                       */
/*       * Tipo valildita' per la scadenza                       */
/*       * - 01 : Scadenza per l'Italia                          */
/*       * - 02 : Scadenza per l'Estero                          */
/*       *-------------------------------------------------------*/
  tvl_sfs                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di scadenza (ex dts-scf)                         */
/*       *-------------------------------------------------------*/
  dat_sca                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice fornitore                                      */
/*       *-------------------------------------------------------*/
  cod_fnt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza del fornitore                       */
/*       *-------------------------------------------------------*/
  dpz_fnt                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamenti al fornitore                        */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla filiale                                   */
/*       *-------------------------------------------------------*/
  inl_scf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per la scadenza                                */
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
/*       * Importo scadenza in valuta                            */
/*       *-------------------------------------------------------*/
  iiv_scf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo scadenza in valuta base                       */
/*       *-------------------------------------------------------*/
  imp_scf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento di riferimento                         */
/*       *-------------------------------------------------------*/
  dat_ddr                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento di riferimento                       */
/*       *-------------------------------------------------------*/
  num_ddr                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento di riferimento                         */
/*       * - 01 : Fattura                                        */
/*       * - 02 : Nota di addebito                               */
/*       * - 11 : Nota di accredito                              */
/*       * - 99 : Non classificato                               */
/*       *-------------------------------------------------------*/
  tip_ddr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo documento di riferimento in valuta            */
/*       *-------------------------------------------------------*/
  imp_ddr                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero rata documento di riferimento                  */
/*       *-------------------------------------------------------*/
  nra_ddr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI banca fornitore per pagamento a mezzo bo-  */
/*       * nifico non in valuta                                  */
/*       *-------------------------------------------------------*/
  abi_fnt                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CAB banca fornitore per pagamento a mezzo bo-  */
/*       * nifico non in valuta                                  */
/*       *-------------------------------------------------------*/
  cab_fnt                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice banca estera del fornitore per pagamento a     */
/*       * mezzo bonifico in valuta                              */
/*       *-------------------------------------------------------*/
  cod_bef                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente bancario del fornitore per pa-  */
/*       * gamento a mezzo bonifico non in valuta                */
/*       *                                                       */
/*       * Codice conto corrente postale del fornitore per pa-   */
/*       * gamento a mezzo c/c postale.                          */
/*       *-------------------------------------------------------*/
  ccc_fnt                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca di appoggio per pagamenti tipo:   */
/*       *  - Incasso Elettronico                                */
/*       *  - Ri.Ba.                                             */
/*       *  - Conferma d'Ordine                                  */
/*       *  - M.Av.                                              */
/*       *  - R.I.D.                                             */
/*       *  - Ricevuta Bancaria                                  */
/*       *  - Tratta                                             */
/*       *                                                       */
/*       * Codice nostra banca di domiciliazione per pagamenti   */
/*       * tipo:                                                 */
/*       *  - Paghero' Cambiario                                 */
/*       *-------------------------------------------------------*/
  cbp_nsb                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No pagamento bloccato per la scadenza              */
/*       *  - N : No                                             */
/*       *  - S : Si'                                            */
/*       *-------------------------------------------------------*/
  snx_pbl                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di storno scadenza       */
/*       *-------------------------------------------------------*/
  dtr_sto                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di storno                               */
/*       *-------------------------------------------------------*/
  drc_sto                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di storno                               */
/*       *-------------------------------------------------------*/
  npc_sto                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di si/no registrazione nuova scadenza a fron- */
/*       * te dello storno                                       */
/*       * - 01 : Si                                             */
/*       * - 02 : No                                             */
/*       *-------------------------------------------------------*/
  ens_sto                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero nuova scadenza a fronte del movimento di stor- */
/*       * no                                                    */
/*       *-------------------------------------------------------*/
  nns_sto                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo nuova scadenza a fronte del movimento di storno  */
/*       *-------------------------------------------------------*/
  tns_sto                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero operazione di pagamento, se scadenza pagata,   */
/*       * formato : s.aa.nnnnnnnn                               */
/*       *-------------------------------------------------------*/
  num_pgf                   bigint  (11)                not null,
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


