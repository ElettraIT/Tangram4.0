/*    *==========================================================*/
/*    * Tabella [oct] Ordini clienti : testate             {orc} */
/*    *                                                          */
/*    * Aggiornamento 31.05.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt     bigint (11)   not null COMMENT 'Protocollo'      ,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento                                        */
/*       *-------------------------------------------------------*/
  cod_tmo     varchar (05)  not null COMMENT 'Tipo movimento'   ,
/*       *-------------------------------------------------------*/
/*       * Codice ns. dipendenza                                 */
/*       *-------------------------------------------------------*/
  cod_dpz     integer (07)  not null COMMENT 'Ns Dipendenza'    ,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *-------------------------------------------------------*/
  dat_doc     date          not null COMMENT 'Data documento'   ,
/*       *-------------------------------------------------------*/
/*       * Numero documento : saa.dd.nnnnnn                      */
/*       *                                                       */
/*       * - s      : secolo                                     */
/*       * - aa     : anno                                       */
/*       * - dd     : codice dipendenza                          */
/*       * - nnnnnn : numero progressivo                         */
/*       *-------------------------------------------------------*/
  num_doc     bigint (11)   not null COMMENT 'Nr documento'     ,
/*       *-------------------------------------------------------*/
/*       * Secolo ed anno della data documento                   */
/*       *                                                       */
/*       * ___ VALUTARE SE NECESSARIO ___                        */
/*       *-------------------------------------------------------*/
  scl_ann                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla numerazione                                     */
/*       *-------------------------------------------------------*/
  sgl_num     varchar (03)  not null COMMENT 'Sigla numerazione',
/*       *-------------------------------------------------------*/
/*       * Tipo di ordine                                        */
/*       *                                                       */
/*       * - Spaces : normale                                    */
/*       * - P      : pro-forma (non impegna il magazzino)       */
/*       * - F      : forecast  (previsionale)                   */
/*       *-------------------------------------------------------*/
  tip_ord     varchar (01)  not null COMMENT 'Tipo ordine'     ,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio per il movimento                        */
/*       *                                                       */
/*       * - C : Cliente                                         */
/*       * - D : Dipendenza                                      */
/*       *-------------------------------------------------------*/
  tip_arc     varchar (01)  not null COMMENT 'Tipo archivio'   ,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *-------------------------------------------------------*/
  cod_arc     integer (07)  not null COMMENT 'Codice archivio' ,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza archivio                            */
/*       *-------------------------------------------------------*/
  dpz_arc     varchar (07)  not null COMMENT 'Sua dipendenza'  ,
/*       *-------------------------------------------------------*/
/*       * Tipo indirizzo di spedizione                          */
/*       *                                                       */
/*       * - 01 : Alla sede legale                               */
/*       * - 02 : Manuale                                        */
/*       * - 03 : Alla sede                                      */
/*       * - 04 : Alla dipendenza                                */
/*       *-------------------------------------------------------*/
  tip_ids     tinyint (02)  not null COMMENT 'Tipo indir. sped',
/*       *-------------------------------------------------------*/
/*       * Data ordine del cliente                               */
/*       *-------------------------------------------------------*/
  ocl_dat     date          not null COMMENT 'Data ord. cliente',
/*       *-------------------------------------------------------*/
/*       * Numero ordine del cliente                             */
/*       *-------------------------------------------------------*/
  ocl_num     varchar (20)  not null COMMENT 'Num. ord. cliente',
/*       *-------------------------------------------------------*/
/*       * Riferimenti ordine del cliente                        */
/*       *-------------------------------------------------------*/
  ocl_rif     varchar (80)  not null COMMENT 'Rif. ord. cliente',
/*       *-------------------------------------------------------*/
/*       * Codice responsabile ordine                            */
/*       *-------------------------------------------------------*/
  cod_rsp     integer (05)  not null COMMENT 'Responsabile'    ,
/*       *-------------------------------------------------------*/
/*       * Data consegna prevista di massima                     */
/*       *-------------------------------------------------------*/
  dat_cns     date          not null COMMENT 'Data consegna ordine',
/*       *-------------------------------------------------------*/
/*       * Formato di stampa data consegna                       */
/*       *                                                       */
/*       * - 01 : Formato gg/mm/aa                               */
/*       * - 02 : Formato mm/aa                                  */
/*       * - 03 : No stampa                                      */
/*       * - 04 : Formato st/aaaa                                */
/*       *-------------------------------------------------------*/
  fds_dtc     tinyint (02)  not null COMMENT 'Formato data consegna',
/*       *-------------------------------------------------------*/
/*       * Tipo di evasione richiesta                            */
/*       *                                                       */
/*       * - 01 : Il totale ordine                               */
/*       * - 02 : Il totale riga                                 */
/*       * - 03 : Il parziale riga                               */
/*       *-------------------------------------------------------*/
  tip_eva     tinyint (02)  not null COMMENT 'Tipo di evasione',
/*       *-------------------------------------------------------*/
/*       * Priorita' di evasione                                 */
/*       *-------------------------------------------------------*/
  pri_eva     varchar (02)  not null COMMENT 'Priorita di evasione',
/*       *-------------------------------------------------------*/
/*       * Codice condizioni di vendita                          */
/*       *-------------------------------------------------------*/
  cod_cdv     integer (03)  not null COMMENT 'Cod. condiz. vendita',
/*       *-------------------------------------------------------*/
/*       * Condizioni di vendita                                 */
/*       *-------------------------------------------------------*/
  des_cdv     text          not null COMMENT 'Testo cond. vendita',
/*       *-------------------------------------------------------*/
/*       * Commento ad uso interno                               */
/*       *-------------------------------------------------------*/
  com_int     text          not null COMMENT 'Commento interno',
/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
/*       *-------------------------------------------------------*/
  cod_lng     varchar (03)  not null COMMENT 'Codice lingua'   ,
/*       *-------------------------------------------------------*/
/*       * Valuta per la fatturazione                            */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vpf     varchar (03)  not null COMMENT 'Valuta fatturazione',
  dec_vpf     tinyint (01)  not null,
  tdc_vpf     varchar (01)  not null,
  cdc_vpf     decimal (12,05)      not null,
/*       *-------------------------------------------------------*/
/*       * Assoggettamento iva per il documento                  */
/*       *                                                       */
/*       * - 00000 : Soggetto iva                                */
/*       * - nnnnn : Titolo di esenzione                         */
/*       *-------------------------------------------------------*/
  ass_iva     integer (05)  not null COMMENT 'Assoggettamento iva',
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contro-     */
/*       * partita vendite                                       */
/*       *                                                       */
/*       * - 0000000 : A seconda delle righe                     */
/*       * - nnnnnnn : Conto di contropartita                    */
/*       *-------------------------------------------------------*/
  ctp_ven     integer (07)  not null COMMENT 'Contropartita vendite',
/*       *-------------------------------------------------------*/
/*       * Fatturazione separata per questo documento            */
/*       *                                                       */
/*       * - N      : No, segue le regole di fatturazione del    */
/*       *            cliente                                    */
/*       * - S      : Si, questo documento deve essere comunque  */
/*       *            fatturato a parte                          */
/*       *-------------------------------------------------------*/
  fat_sep     varchar (01)  not null COMMENT 'Fattura separata',
/*       *-------------------------------------------------------*/
/*       * Inoltro documento                                     */
/*       *                                                       */
/*       * - 01 : Alla sede                                      */
/*       * - 02 : Alla sede legale                               */
/*       * - 03 : Alla dipendenza del cliente                    */
/*       *-------------------------------------------------------*/
  inl_dcm     tinyint (02)  not null COMMENT 'Inoltro documento',
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamenti                                     */
/*       *                                                       */
/*       * - 01 : Alla sede                                      */
/*       * - 02 : Alla sede legale                               */
/*       * - 03 : Alla dipendenza del cliente                    */
/*       *-------------------------------------------------------*/
  inl_pgt     tinyint (02)  not null COMMENT 'Inoltro pagamenti',
/*       *-------------------------------------------------------*/
/*       * Codice listino da applicare                           */
/*       *                                                       */
/*       * - Spaces : listino base                               */
/*       * - xxx    : listino xxx                                */
/*       *-------------------------------------------------------*/
  cod_lst     varchar (03)  not null COMMENT 'Codice listino'  ,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in riga associata al cliente         */
/*       *-------------------------------------------------------*/
  csr_aac     integer (05)  not null COMMENT 'Cat. sconto riga cli.',
/*       *-------------------------------------------------------*/
/*       * % sconto in riga associata al cliente                 */
/*       *-------------------------------------------------------*/
  psr_ac1                   decimal (05,02)             not null,
  psr_ac2                   decimal (05,02)             not null,
  psr_ac3                   decimal (05,02)             not null,
  psr_ac4                   decimal (05,02)             not null,
  psr_ac5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in chiusura associata al cliente     */
/*       *-------------------------------------------------------*/
  csc_aac     integer (05)  not null COMMENT 'Cat. sconto chiusura cli.',
/*       *-------------------------------------------------------*/
/*       * % sconto chiusura associato al cliente                */
/*       *-------------------------------------------------------*/
  psc_aac                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Categoria di provvigioni associata al cliente         */
/*       *-------------------------------------------------------*/
  cpv_aac     integer (05)  not null COMMENT 'Cat. provvigioni cli.',
/*       *-------------------------------------------------------*/
/*       * % di provvigione associate al cliente                 */
/*       *-------------------------------------------------------*/
  ppv_ac1                   decimal (05,02)             not null,
  ppv_ac2                   decimal (05,02)             not null,
  ppv_ac3                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Forma di pagamento per il documento                   */
/*       *-------------------------------------------------------*/
  cod_fop     integer (07)  not null COMMENT 'Forma di pagamento',
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
/*       * cumento.                                              */
/*       *-------------------------------------------------------*/
  ccc_app     varchar (20)  not null COMMENT 'C/C cliente'  ,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca per pagamenti per mezzo bonifico  */
/*       * bancario                                              */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  nos_ban     varchar (10)  not null COMMENT 'Nostra banca'  ,
/*       *-------------------------------------------------------*/
/*       * Codice nostro c/c postale per pagamenti per mezzo c/c */
/*       * postale                                               */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  nos_ccp     varchar (20)  not null COMMENT 'C/C nostro'   ,
/*       *-------------------------------------------------------*/
/*       * Codice addebito spese incasso                         */
/*       *-------------------------------------------------------*/
  add_spi     varchar (03)  not null COMMENT 'Codice spese incasso',
/*       *-------------------------------------------------------*/
/*       * Codice addebito spese bollo                           */
/*       *-------------------------------------------------------*/
  add_spb     varchar (03)  not null COMMENT 'Codice spese bollo',
/*       *-------------------------------------------------------*/
/*       * Tipo incasso alternativo per incassi elettronici,     */
/*       * normalmente destinati a risolversi in una dispo-      */
/*       * sizione di pagamento                                  */
/*       *                                                       */
/*       * - 01 : Nessuna preferenza                             */
/*       * - 02 : Ri.Ba.                                         */
/*       * - 03 : M.av.                                          */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *                                                       */
/*       * ___ ELIMINARE ??? ___                                 */
/*       *-------------------------------------------------------*/
  ipr_iel                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data scadenza pagamento nel caso che la forma di pa-  */
/*       * gamento preveda un codice di pagamento con data di    */
/*       * decorrenza manuale                                    */
/*       *                                                       */
/*       * ___ VALUTARE IN BASE ALLA FORMA DI PAGAMENTO ___      */
/*       *-------------------------------------------------------*/
  pag_dsm     date          not null COMMENT 'Data scadenza manuale',
/*       *-------------------------------------------------------*/
/*       * Ammontare della quota a forfait nel caso che la forma */
/*       * di pagamento preveda un codice di pagamento relativa  */
/*       * ad una quota a forfait, in valuta                     */
/*       *                                                       */
/*       * ___ ELIMINARE ??? ___                                 */
/*       *-------------------------------------------------------*/
  pag_qaf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare dell'acconto nel caso che la forma di paga- */
/*       * mento preveda la deduzione dell'acconto relativamente */
/*       * ad uno dei tre codici di pagamento previsti,          */
/*       * in valuta                                             */
/*       *-------------------------------------------------------*/
  pag_act       bigint (13)   not null COMMENT 'Acconto'          ,
/*       *-------------------------------------------------------*/
/*       * Codice agente per le provvigioni sul documento        */
/*       *-------------------------------------------------------*/
  cod_age     integer (07)  not null COMMENT 'Codice agente'    ,
/*       *-------------------------------------------------------*/
/*       * Flag di significativita' provvigioni per il documento */
/*       *                                                       */
/*       * - 01 : Si, da righe documento                         */
/*       * - 02 : No provvigioni per il documento                */
/*       * - 03 : A forfait per l'intero documento               */
/*       * - 04 : A forfait per l'intero documento, in %         */
/*       *-------------------------------------------------------*/
  fsp_doc     tinyint (02)  not null COMMENT 'Signif. provvigioni',
/*       *-------------------------------------------------------*/
/*       * Ammontare provvigioni a forfait per il documento per  */
/*       * l'agente, in valuta base                              */
/*       *                                                       */
/*       * N.B.: Significativo solo se campo precedente a 03     */
/*       *-------------------------------------------------------*/
  pvf_age     bigint (13)   not null COMMENT 'provvigioni a forfait',
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
  tip_vpa     tinyint (02)  not null COMMENT 'Vendita diretta/ind',
/*       *-------------------------------------------------------*/
/*       * Categoria di provvigioni associata all'agente         */
/*       *-------------------------------------------------------*/
  cpv_aaa     integer (05)  not null COMMENT 'Cat. provvigioni age.',
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
/*       *                                                       */
/*       * ___ ELIMINARE ??? ___                                 */
/*       *-------------------------------------------------------*/
  cod_ime                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare provvigioni a forfait per il documento per  */
/*       * l'intermediario, in valuta base                       */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *                                                       */
/*       * ___ ELIMINARE ??? ___                                 */
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
/*       * - 09 : Totale extra                                   */
/*       *-------------------------------------------------------*/
  tot_r01     bigint (13)   not null COMMENT 'Totale merce'    ,
  tot_r02     bigint (13)   not null COMMENT 'Totale servizi'  ,
  tot_r03     bigint (13)   not null COMMENT 'Totale imballi'  ,
  tot_r04     bigint (13)   not null COMMENT 'Totale libero'   ,
  tot_r05     bigint (13)   not null COMMENT 'Totale libero'   ,
  tot_r06     bigint (13)   not null COMMENT 'Totale libero'   ,
  tot_r07     bigint (13)   not null COMMENT 'Totale libero'   ,
  tot_r08     bigint (13)   not null COMMENT 'Totale libero'   ,
  tot_r09     bigint (13)   not null COMMENT 'Totale extra'    ,
/*       *-------------------------------------------------------*/
/*       * Importo sconto in chiusura, in valuta                 */
/*       *-------------------------------------------------------*/
  tot_scc     bigint (13)   not null COMMENT 'Totale sconto chius.',
/*       *-------------------------------------------------------*/
/*       * % sconto in chiusura                                  */
/*       *-------------------------------------------------------*/
  per_scc                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Importo sconto pagamento, in valuta                   */
/*       *-------------------------------------------------------*/
  tot_scp     bigint (13)   not null COMMENT 'Totale sconto pagamento',
/*       *-------------------------------------------------------*/
/*       * % sconto sconto pagamento                             */
/*       *-------------------------------------------------------*/
  per_scp                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Totale documento, in valuta                           */
/*       *-------------------------------------------------------*/
  tot_doc     bigint (13)   not null COMMENT 'Totale doc. in valuta',
/*       *-------------------------------------------------------*/
/*       * Numero di stampe eseguite                             */
/*       *                                                       */
/*       * ___ ELIMINARE ??? ___                                 */
/*       *-------------------------------------------------------*/
  ctr_stp                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di ordine verificato                             */
/*       *                                                       */
/*       * - 0 : Verificato                                      */
/*       * - 1 : Da verificare                                   */
/*       *-------------------------------------------------------*/
  flg_ver     tinyint (01)  not null COMMENT 'Verifica ordine 0/1',
/*       *-------------------------------------------------------*/
/*       * Flag di stampa prezzi e sconti.                       */
/*       *                                                       */
/*       * - 0 : Non stampa prezzi e sconti                      */
/*       * - 1 : Segue le regole del tipo documento              */
/*       *-------------------------------------------------------*/
  flg_sns     tinyint (01)  not null COMMENT 'Stampa prezzi 0/1',
/*       *-------------------------------------------------------*/
/*       * Flag di ordine completamente chiuso                   */
/*       *                                                       */
/*       * - 0 : Ordine aperto                                   */
/*       * - 1 : Ordine chiuso                                   */
/*       *-------------------------------------------------------*/
  flg_chs     tinyint (01)  not null COMMENT 'Chiusura ordine 0/1',
/*       *-------------------------------------------------------*/
/*       * Flags di elaborazione bloccanti                       */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_bl1                   tinyint (01)                not null,
  flg_bl2                   tinyint (01)                not null,
  flg_bl3                   tinyint (01)                not null,
  flg_bl4                   tinyint (01)                not null,
  flg_bl5                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di elaborazione NON bloccanti                   */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   tinyint (01)                not null,
  flg_nb2                   tinyint (01)                not null,
  flg_nb3                   tinyint (01)                not null,
  flg_nb4                   tinyint (01)                not null,
  flg_nb5                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia effettuabile                         */
/*       *-------------------------------------------------------*/
  flg_pul                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
