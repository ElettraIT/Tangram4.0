/*    *==========================================================*/
/*    * Tabella [ost] Ordini di spedizione : testate       {ods} */
/*    *                                                          */
/*    * Aggiornamento 31.05.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                001 *
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null COMMENT 'Numero protocollo',
/*       *-------------------------------------------------------*/
/*       * Tipo movimento                                    002 *
/*       *-------------------------------------------------------*/
  cod_tmo                   varchar (05)                not null COMMENT 'Tipo movimento',
/*       *-------------------------------------------------------*/
/*       * Codice nostra dipendenza                          003 */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null COMMENT 'Codice nostra dipendenza',
/*       *-------------------------------------------------------*/
/*       * Data documento                                    004 */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null COMMENT 'Data documento',
/*       *-------------------------------------------------------*/
/*       * Tipo archivio per il movimento                    005 */
/*       *                                                       */
/*       * - N : No tipo archivio                                */
/*       * - C : Cliente                                         */
/*       * - F : Fornitore                                       */
/*       * - D : Deposito                                        */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null COMMENT 'Tipo archivio',
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                   006 */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null COMMENT 'Codice archivio',
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza archivio                        007 */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null COMMENT 'Codice dipendenza archivio',
/*       *-------------------------------------------------------*/
/*       * Tipo indirizzo di spedizione                      008 */
/*       *                                                       */
/*       * - 01 : alla sede legale                               */
/*       * - 02 : manuale                                        */
/*       * - 03 : alla sede                                      */
/*       * - 04 : alla dipendenza                                */
/*       *-------------------------------------------------------*/
  tip_ids                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua per il documento                    009 */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per la fatturazione                        010 */
/*       *                                                   013 */
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
/*       * Assoggettamento iva per il documento              014 */
/*       *                                                       */
/*       * - 00000 : Soggetto iva                                */
/*       * - nnnnn : Titolo di esenzione                         */
/*       *-------------------------------------------------------*/
  ass_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contro-     */
/*       * partita vendite                                       */
/*       *                                                       */
/*       * - 0000000 : A seconda delle righe                     */
/*       * - nnnnnnn : Conto di contropartita                    */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Fatturazione separata per questo documento        016 */
/*       *                                                       */
/*       * - N      : No, segue le regole di fatturazione del    */
/*       *            cliente                                    */
/*       * - S      : Si, questo documento deve essere comunque  */
/*       *            fatturato a parte                          */
/*       * - O      : Si, questo documento puo' essere fatturato */
/*       *            solo con altri che si riferiscono allo     */
/*       *            stesso ordine                              */
/*       *-------------------------------------------------------*/
  fat_sep                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro documenti                                 017 */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla filiale                                   */
/*       * - 99 : secondo dati anagrafici dedotti da [dcc]       */
/*       *                                                       */
/*       * N.B.: attualmente il campo non viene gestito ma viene */
/*       *       forzato al valore 99                            */
/*       *-------------------------------------------------------*/
  inl_dcm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamenti                                 018 */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla filiale                                   */
/*       *-------------------------------------------------------*/
  inl_pgt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice listino da applicare per il documento      019 */
/*       *                                                       */
/*       * - Spaces : listino base                               */
/*       * - xxx    : listino xxx                                */
/*       *-------------------------------------------------------*/
  cod_lst                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in riga associata al cliente     020 */
/*       *-------------------------------------------------------*/
  csr_aac                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto in riga associate al cliente       021..025  */
/*       *-------------------------------------------------------*/
  psr_ac1                   decimal (05,02)             not null,
  psr_ac2                   decimal (05,02)             not null,
  psr_ac3                   decimal (05,02)             not null,
  psr_ac4                   decimal (05,02)             not null,
  psr_ac5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in chiusura associata al cliente 026 */
/*       *-------------------------------------------------------*/
  csc_aac                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto chiusura associato al cliente            027 */
/*       *-------------------------------------------------------*/
  psc_aac                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Categoria di provvigioni associata al cliente     028 */
/*       *-------------------------------------------------------*/
  cpv_aac                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % di provvigione associate al cliente        029..031 */
/*       *-------------------------------------------------------*/
  ppv_ac1                   decimal (05,02)             not null,
  ppv_ac2                   decimal (05,02)             not null,
  ppv_ac3                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Forma di pagamento per il documento               032 */
/*       *-------------------------------------------------------*/
  cod_fop                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sconto pagamento associato alla forma di pagamento    */
/*       *-------------------------------------------------------*/
  scp_aap                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI dell'istituto di credito di appoggio per   */
/*       * gli effetti, per il documento                         */
/*       *-------------------------------------------------------*/
  cod_abi                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CAB per lo sportello dell'istituto di credito  */
/*       * di appoggio per gli effetti, per il documento         */
/*       *-------------------------------------------------------*/
  cod_cab                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente del cliente presso l'istituto   */
/*       * di credito per l'appoggio del pagamento, solo per gli */
/*       * accrediti pre-autorizzati in conto 'RID', per il do-  */
/*       * cumento.                                          036 */
/*       *-------------------------------------------------------*/
  ccc_app                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca per pagamenti per mezzo bonifico  */
/*       * bancario                                              */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  nos_ban                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostro c/c postale per pagamenti per mezzo c/c */
/*       * postale                                               */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  nos_ccp                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice addebito spese incasso                         */
/*       *-------------------------------------------------------*/
  add_spi                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice addebito spese bollo                       040 */
/*       *-------------------------------------------------------*/
  add_spb                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo incasso alternativo per incassi elettronici, 041 */
/*       * normalmente destinati a risolversi in una dispo-      */
/*       * sizione di pagamento                                  */
/*       *                                                       */
/*       * - 01 : Nessuna preferenza                             */
/*       * - 02 : Ri.Ba.                                         */
/*       * - 03 : M.av.                                          */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  ipr_iel                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data scadenza pagamento nel caso che la forma di pa-  */
/*       * gamento preveda un codice di pagamento con data di    */
/*       * decorrenza manuale                                    */
/*       *-------------------------------------------------------*/
  pag_dsm                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare della quota a forfait nel caso che la forma */
/*       * di pagamento preveda un codice di pagamento relativa  */
/*       * ad una quota a forfait, in valuta                     */
/*       *-------------------------------------------------------*/
  pag_qaf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare dell'acconto nel caso che la forma di paga- */
/*       * mento preveda la deduzione dell'acconto relativamente */
/*       * ad uno dei tre codici di pagamento previsti,          */
/*       * in valuta                                             */
/*       *-------------------------------------------------------*/
  pag_act                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente per le provvigioni sul documento    045 */
/*       *-------------------------------------------------------*/
  cod_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di significativita' provvigioni per il documento */
/*       *                                                       */
/*       * - 01 : Si, da righe documento                         */
/*       * - 02 : No provvigioni per il documento                */
/*       * - 03 : A forfait per l'intero documento               */
/*       * - 04 : A forfait per l'intero documento, in %     046 */
/*       *-------------------------------------------------------*/
  fsp_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare provvigioni a forfait per il documento per  */
/*       * l'agente, in valuta base                              */
/*       *                                                       */
/*       * N.B.: Significativo solo se campo precedente a 03     */
/*       *-------------------------------------------------------*/
  pvf_age                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare provvigioni a forfait per il documento per  */
/*       * l'agente, in %                                        */
/*       *                                                       */
/*       * N.B.: Significativo solo se campo precedente a 03     */
/*       *-------------------------------------------------------*/
  ppf_age                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Tipo vendita per l'agente                             */
/*       *                                                       */
/*       * - 01 : Diretta                                        */
/*       * - 02 : Indiretta                                      */
/*       *                                                       */
/*       * N.B.: Significativo solo se codice agente diverso da  */
/*       *       zero                                            */
/*       *-------------------------------------------------------*/
  tip_vpa                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria di provvigioni associata all'agente     050 */
/*       *-------------------------------------------------------*/
  cpv_aaa                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % di provvigioni associate all'agente                 */
/*       *-------------------------------------------------------*/
  ppv_aa1                   decimal (05,02)             not null,
  ppv_aa2                   decimal (05,02)             not null,
  ppv_aa3                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice intermediario per le provvigioni sul documento */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  cod_ime                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare provvigioni a forfait per il documento per  */
/*       * l'intermediario, in valuta base                       */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito              055  */
/*       *-------------------------------------------------------*/
  pvf_ime                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Totali di suddivisione per le righe, in valuta        */
/*       *                                                       */
/*       * - 01 : Totale merce                                   */
/*       * - 02 : Totale servizi                                 */
/*       * - 03 : Totale imballi                                 */
/*       * - 04 : Totale libero 4                                */
/*       * - 05 : Totale libero 5                                */
/*       * - 06 : Totale libero 6                                */
/*       * - 07 : Totale libero 7                                */
/*       * - 08 : Totale libero 8                                */
/*       * - 09 : Totale extra                          056..064 */
/*       *-------------------------------------------------------*/
  tot_r01                   bigint  (13)                not null,
  tot_r02                   bigint  (13)                not null,
  tot_r03                   bigint  (13)                not null,
  tot_r04                   bigint  (13)                not null,
  tot_r05                   bigint  (13)                not null,
  tot_r06                   bigint  (13)                not null,
  tot_r07                   bigint  (13)                not null,
  tot_r08                   bigint  (13)                not null,
  tot_r09                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo sconto in chiusura, in valuta             065 */
/*       *-------------------------------------------------------*/
  tot_scc                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto in chiusura                              066 */
/*       *-------------------------------------------------------*/
  per_scc                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Importo sconto pagamento, in valuta               067 */
/*       *-------------------------------------------------------*/
  tot_scp                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto sconto pagamento                         068 */
/*       *-------------------------------------------------------*/
  per_scp                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Totale documento, in valuta                       069 */
/*       *-------------------------------------------------------*/
  tot_doc                   bigint  (13)                not null,
/*       *=======================================================*/
/*       * TRASPORTO                                             */
/*       *-------------------------------------------------------*/
/*       *-------------------------------------------------------*/
/*       * Trasporto a cura                                  070 */
/*       *                                                       */
/*       * - 00 : Non specificato                                */
/*       * - 10 : Mittente                                       */
/*       * - 20 : Destinatario                                   */
/*       * - 30 : Vettore                                        */
/*       *-------------------------------------------------------*/
  tra_cur                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Causale del trasporto                                 */
/*       *-------------------------------------------------------*/
  cau_tra                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice aspetto esteriore dei beni                     */
/*       *                                                       */
/*       * N.B. : informazione attualmente non obbligatoria      */
/*       *-------------------------------------------------------*/
  asp_ben                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice aspetto esteriore dei beni                     */
/*       *                                                       */
/*       * N.B. : informazione attualmente non obbligatoria      */
/*       *-------------------------------------------------------*/
  num_col                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Peso totale                                       074 */
/*       *                                                       */
/*       * N.B. : informazione attualmente non obbligatoria      */
/*       *-------------------------------------------------------*/
  pes_tot                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Data inizio trasporto                                 */
/*       *                                                       */
/*       * N.B. : informazione attualmente non obbligatoria      */
/*       *-------------------------------------------------------*/
  dat_itr                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Ora inizio trasporto                                  */
/*       *                                                       */
/*       * N.B. : informazione attualmente non obbligatoria      */
/*       *-------------------------------------------------------*/
  ora_itr                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice primo vettore                              077 */
/*       *-------------------------------------------------------*/
  cod_vet                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice secondo vettore                                */
/*       *-------------------------------------------------------*/
  cod_vt2                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice terzo vettore                                  */
/*       *-------------------------------------------------------*/
  cod_vt3                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice annotazioni                                080 */
/*       *-------------------------------------------------------*/
  cod_ant                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * TRASPORTO - fine                                      */
/*       *=======================================================*/
/*       *-------------------------------------------------------*/
/*       * Numero di stampe eseguite, max 99                 081 */
/*       *-------------------------------------------------------*/
  ctr_stp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di spedizione completamente chiusa           082 */
/*       *                                                       */
/*       * - Spaces : Non ancora chiusa                          */
/*       * - #      : Completamente chiusa                       */
/*       *-------------------------------------------------------*/
  flg_chs                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags bloccanti                              083..089 */
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
/*       * Flags non bloccanti                          090..092 */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   varchar (01)                not null,
  flg_nb2                   varchar (01)                not null,
  flg_nb3                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia effettuabile                     093 */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di regime fiscale particolare                094 */
/*       *                                                       */
/*       * - Spaces o N : Normale regime fiscale                 */
/*       * - S          : Regime di Scissione Pagamenti          */
/*       *-------------------------------------------------------*/
  flg_rfp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo di magazzino                    095 */
/*       *                                                       */
/*       * N.B.: Campo attualmente gestito per i movimenti di    */
/*       *       magazzino relativi alle ubicazioni.             */
/*       *                                                       */
/*       *       VEDERE SE NECESSARIO Ereditare nelle righe,     */
/*       *                            nel campo 'rf-bfr-orm-prt' */
/*       *-------------------------------------------------------*/
  prt_mag                   bigint  (11)                not null COMMENT 'Protocollo di magazzino',
/*       *-------------------------------------------------------*/
/*       * Codice responsabile di magazzino                  096 */
/*       *-------------------------------------------------------*/
  cod_rsm     integer (05)  not null COMMENT 'Responsabile mag'    ,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento per la bollettazione di default    097 */
/*       *-------------------------------------------------------*/
  bcd_tip                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                               098..100 */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
