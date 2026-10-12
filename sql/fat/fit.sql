/*    *==========================================================*/
/*    * Tabella [fit] Fatture clienti : testate            {fat} */
/*    *                                                          */
/*    * Aggiornamento 01.10.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento                                        */
/*       *-------------------------------------------------------*/
  cod_tmo                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento associato al tipo movimento            */
/*       *                                                       */
/*       * - 01 : Fattura                                        */
/*       * - 02 : Nota di addebito                               */
/*       * - 03 : Nota di accredito                              */
/*       * - 04 : Fattura Pro-Forma                              */
/*       *                                                       */
/*       * N.B. : campo proveniente dal tipo movimento           */
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
/*       *                                                       */
/*       * N.B. : campo proveniente dal tipo movimento           */
/*       *-------------------------------------------------------*/
  org_doc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di giornale iva per il documento associato al  */
/*       * tipo movimento                                        */
/*       *                                                       */
/*       * N.B. : campo proveniente dal tipo movimento           */
/*       *-------------------------------------------------------*/
  num_giv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra dipendenza che ha emesso il documento   */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento : s.aa.xx.nnnnnn                     */
/*       *                                                       */
/*       * - s      : secolo                                     */
/*       * - aa     : anno                                       */
/*       * - xx     : numero giornale iva                        */
/*       * - nnnnnn : numero progressivo                         */
/*       *-------------------------------------------------------*/
  num_doc                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Secolo ed anno relativi alla data documento           */
/*       *-------------------------------------------------------*/
  scl_ann                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla numerazione relativa al numero documento        */
/*       *                                                       */
/*       * N.B. : campo proveniente dal tipo movimento           */
/*       *-------------------------------------------------------*/
  sgl_num                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice cliente                                        */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza cliente                             */
/*       *-------------------------------------------------------*/
  dpz_cli                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua per il documento                        */
/*       *                                                       */
/*       * - Se tip-frn = 01 : Da cliente commerciale            */
/*       * - Se tip-frn = 11 : Da cliente commerciale            */
/*       * - Se tip-frn = 21 : Da cliente per la fatturazione    */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per la fatturazione                            */
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
/*       * Assoggettamento iva per il documento                  */
/*       *                                                       */
/*       * - 00000 : Soggetto iva                                */
/*       * - nnnnn : Titolo di esenzione                         */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  ass_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contro-     */
/*       * partita vendite                                       */
/*       *                                                       */
/*       * - 0000000 : A seconda delle righe                     */
/*       * - nnnnnnn : Conto di contropartita                    */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro documenti                                     */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla sua dipendenza                            */
/*       *                                                       */
/*       * In caso di fattura accompagnatoria viene forzato al   */
/*       * valore 99                                             */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  inl_dcm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamenti                                     */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla sua dipendenza                            */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  inl_pgt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice listino da applicare per il documento          */
/*       *                                                       */
/*       * - Spaces : listino base                               */
/*       * - xxx    : listino xxx                                */
/*       *                                                       */
/*       * - Se tip-frn = 01 : Da cliente commerciale            */
/*       * - Se tip-frn = 11 : Da cliente commerciale            */
/*       * - Se tip-frn = 21 : Da cliente per la fatturazione    */
/*       *-------------------------------------------------------*/
  cod_lst                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in riga associata al cliente         */
/*       *                                                       */
/*       * - Se tip-frn = 01 : Da cliente commerciale            */
/*       * - Se tip-frn = 11 : Da cliente commerciale            */
/*       * - Se tip-frn = 21 : Da cliente per la fatturazione    */
/*       *-------------------------------------------------------*/
  csr_aac                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto in riga associate al cliente                 */
/*       *                                                       */
/*       * - Se tip-frn = 01 : Da cliente commerciale            */
/*       * - Se tip-frn = 11 : Da cliente commerciale            */
/*       * - Se tip-frn = 21 : Da cliente per la fatturazione    */
/*       *-------------------------------------------------------*/
  psr_ac1                   decimal (05,02)             not null,
  psr_ac2                   decimal (05,02)             not null,
  psr_ac3                   decimal (05,02)             not null,
  psr_ac4                   decimal (05,02)             not null,
  psr_ac5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in chiusura associata al cliente     */
/*       *                                                       */
/*       * - Se tip-frn = 01 : Da cliente commerciale            */
/*       * - Se tip-frn = 11 : Da cliente commerciale            */
/*       * - Se tip-frn = 21 : Da cliente per la fatturazione    */
/*       *-------------------------------------------------------*/
  csc_aac                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto chiusura associato al cliente                */
/*       *                                                       */
/*       * - Se tip-frn = 01 : Da cliente commerciale            */
/*       * - Se tip-frn = 11 : Da cliente commerciale            */
/*       * - Se tip-frn = 21 : Da cliente per la fatturazione    */
/*       *-------------------------------------------------------*/
  psc_aac                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Categoria di provvigioni associata al cliente         */
/*       *-------------------------------------------------------*/
  cpv_aac                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % di provvigione associate al cliente                 */
/*       *-------------------------------------------------------*/
  ppv_ac1                   decimal (05,02)             not null,
  ppv_ac2                   decimal (05,02)             not null,
  ppv_ac3                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Forma di pagamento per il documento                   */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  cod_fop                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sconto pagamento associato alla forma di pagamento    */
/*       *-------------------------------------------------------*/
  scp_aap                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI dell'istituto di credito di appoggio per   */
/*       * gli effetti, per il documento                         */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  cod_abi                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CAB per lo sportello dell'istituto di credito  */
/*       * di appoggio per gli effetti, per il documento         */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
/*       *-------------------------------------------------------*/
  cod_cab                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente del cliente presso l'istituto   */
/*       * di credito per l'appoggio del pagamento, solo per gli */
/*       * accrediti pre-autorizzati in conto 'RID', per il do-  */
/*       * cumento.                                              */
/*       *                                                       */
/*       * Da cliente per fatturazione                           */
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
/*       * Codice addebito spese bollo                           */
/*       *-------------------------------------------------------*/
  add_spb                   varchar (03)                not null,
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
/*       * Codice agente per le provvigioni sul documento        */
/*       *                                                       */
/*       * - Se tip-frn = 01 : Da cliente commerciale            */
/*       * - Se tip-frn = 11 : Da cliente commerciale            */
/*       * - Se tip-frn = 21 : Da cliente commerciale, se c'e',  */
/*       *                     altrimenti da cliente per la      */
/*       *                     fatturazione                      */
/*       *-------------------------------------------------------*/
  cod_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di significativita' provvigioni per il documento */
/*       *                                                       */
/*       * - 01 : Si, da righe documento                         */
/*       * - 02 : No provvigioni per il documento                */
/*       * - 03 : A forfait per l'intero documento               */
/*       * - 04 : A forfait per l'intero documento, in %         */
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
/*       * Categoria di provvigioni associata all'agente         */
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
/*       * N.B.: Campo attualmente non gestito                   */
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
/*       * Importo sconto in chiusura, in valuta                 */
/*       *-------------------------------------------------------*/
  tot_scc                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto in chiusura                                  */
/*       *-------------------------------------------------------*/
  per_scc                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per l'importo sconto in chiusura           */
/*       *                                                       */
/*       * N.B. : Se a zero significa che l'importo non e' sog-  */
/*       *        getto ad un particolare codice iva, ma che     */
/*       *        l'importo va' sventagliato in proporzione ai   */
/*       *        codice iva gia' presenti nel documento         */
/*       *-------------------------------------------------------*/
  civ_scc                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per l'importo sconto in chiusura */
/*       *                                                       */
/*       * N.B. : Se a zero significa che l'importo non si rife- */
/*       *        risce ad una particolare contropartita, ma che */
/*       *        l'importo va' sventagliato in proporzione alle */
/*       *        contropartite gia' presenti nel documento      */
/*       *-------------------------------------------------------*/
  ccp_scc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo sconto pagamento, in valuta                   */
/*       *-------------------------------------------------------*/
  tot_scp                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto sconto pagamento                             */
/*       *-------------------------------------------------------*/
  per_scp                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per l'importo sconto pagamento             */
/*       *                                                       */
/*       * N.B. : Se a zero significa che l'importo non e' sog-  */
/*       *        getto ad un particolare codice iva, ma che     */
/*       *        l'importo va' sventagliato in proporzione ai   */
/*       *        codice iva gia' presenti nel documento         */
/*       *-------------------------------------------------------*/
  civ_scp                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per l'importo sconto pagamento   */
/*       *                                                       */
/*       * N.B. : Se a zero significa che l'importo non si rife- */
/*       *        risce ad una particolare contropartita, ma che */
/*       *        l'importo va' sventagliato in proporzione alle */
/*       *        contropartite gia' presenti nel documento      */
/*       *-------------------------------------------------------*/
  ccp_scp                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice addebito spese incasso                         */
/*       *-------------------------------------------------------*/
  add_spi                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per le spese incasso                       */
/*       *                                                       */
/*       * N.B. : Se a zero significa che la spesa non e' sog-   */
/*       *        getta ad un particolare codice iva, ma che     */
/*       *        l'importo va' sventagliato in proporzione ai   */
/*       *        codice iva gia' presenti nel documento         */
/*       *-------------------------------------------------------*/
  civ_spi                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per le spese incasso             */
/*       *                                                       */
/*       * N.B. : Se a zero significa che la spesa non si rife-  */
/*       *        risce ad una particolare contropartita, ma che */
/*       *        l'importo va' sventagliato in proporzione alle */
/*       *        contropartite gia' presenti nel documento      */
/*       *-------------------------------------------------------*/
  ccp_spi                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese incasso calcolate, valuta               */
/*       *-------------------------------------------------------*/
  tot_sic                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese incasso aggiuntive, valuta              */
/*       *-------------------------------------------------------*/
  tot_sia                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese bollo, in valuta                        */
/*       *-------------------------------------------------------*/
  tot_spb                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per le spese bollo                         */
/*       *                                                       */
/*       * N.B. : Se a zero significa che la spesa non e' sog-   */
/*       *        getta ad un particolare codice iva, ma che     */
/*       *        l'importo va' sventagliato in proporzione ai   */
/*       *        codice iva gia' presenti nel documento         */
/*       *-------------------------------------------------------*/
  civ_spb                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per le spese bollo               */
/*       *                                                       */
/*       * N.B. : Se a zero significa che la spesa non si rife-  */
/*       *        risce ad una particolare contropartita, ma che */
/*       *        l'importo va' sventagliato in proporzione alle */
/*       *        contropartite gia' presenti nel documento      */
/*       *-------------------------------------------------------*/
  ccp_spb                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo per movimento di contabilita' gene- */
/*       * rale relativo al documento                            */
/*       *                                                       */
/*       * N.B. : La data di registrazione per il movimento di   */
/*       *        contabilita' generale relativo al documento e' */
/*       *        sempre pari alla data di registrazione         */
/*       *-------------------------------------------------------*/
  prt_mgd                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero registrazioni di contabilita' generale genera- */
/*       * te per il documento                                   */
/*       *-------------------------------------------------------*/
  nrg_mgd                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di riferimento per movimento di contabilita' ge- */
/*       * nerale relativo al documento                          */
/*       *                                                       */
/*       * N.B.: Campo attualmente utilizzato per ospitare la    */
/*       *       data documento del cliente nel caso di bolla di */
/*       *       reso su vendita cui segue Nota di Accredito     */
/*       *-------------------------------------------------------*/
  dri_mgd                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero di riferimento per movimento di contabilita'   */
/*       * generale relativo al documento                        */
/*       *                                                       */
/*       * N.B.: Campo attualmente utilizzato per ospitare il    */
/*       *       numero documento del cliente nel caso di bolla  */
/*       *       di reso su vendita cui segue Nota di Accredito  */
/*       *-------------------------------------------------------*/
  nri_mgd                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero scadenza della prima scadenza generata per la  */
/*       * gestione del portafoglio effetti passivi              */
/*       *                                                       */
/*       * N.B. : Le scadenze successive alla prima hanno un nu- */
/*       *        mero scadenza in progressione di uno in uno    */
/*       *-------------------------------------------------------*/
  nps_sdb                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di scadenze generate per la gestione del por-  */
/*       * tafoglio effetti attivi : 00..96                      */
/*       *-------------------------------------------------------*/
  ctr_sdb                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Totale documento, in valuta                           */
/*       *-------------------------------------------------------*/
  iva_tdo                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di stampe eseguite, max 99                     */
/*       *-------------------------------------------------------*/
  ctr_stp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di documento verificato                          */
/*       *                                                       */
/*       * - 0 : Verificato                                      */
/*       * - 1 : Da verificare                                   */
/*       *-------------------------------------------------------*/
  flg_ver     tinyint (01)  not null COMMENT 'Verifica documento 0/1',
/*       *-------------------------------------------------------*/
/*       * Flag di fattura completamente chiusa                  */
/*       *                                                       */
/*       * - Spaces : Non ancora chiusa                          */
/*       * - #      : Completamente chiusa                       */
/*       *-------------------------------------------------------*/
  flg_chs                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags bloccanti                                       */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : sottoposizione ad emissione gestione agenti      */
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
/*       * 01 : Flag di esclusione dalle statistiche             */
/*       *       - Spaces : no esclusione                        */
/*       *       - '#'    : esclusione                           */
/*       *                                                       */
/*       *      --------------------------------------------     */
/*       * 02 : Flag di fattura in attesa di verifica            */
/*       *                                                       */
/*       *       - Spaces : verificato                           */
/*       *       - '#'    : in attesa di verifica                */
/*       *                                                       */
/*       *      N.B.: Soggetto a personalizzazione               */
/*       *      --------------------------------------------     */
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
/*       * Flag di regime fiscale particolare                    */
/*       *                                                       */
/*       * - Spaces o N : Normale regime fiscale                 */
/*       * - S          : Regime di Scissione Pagamenti          */
/*       *-------------------------------------------------------*/
  flg_rfp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di documento con ritenuta d'acconto              */
/*       *                                                       */
/*       * - Spaces o N : Nessuna ritenuta                       */
/*       * - S          : Ritenuta d'acconto                     */
/*       *-------------------------------------------------------*/
  flg_rda                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Percentuale di ritenuta                               */
/*       *-------------------------------------------------------*/
  per_rda                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Percentuale di imponibile su cui applicare la ritenu- */
/*       * ta                                                    */
/*       *                                                       */
/*       * N.B. : Se flag di ritenuta a 'S' e presente % di ri-  */
/*       *        tenuta, se % imponibile a zero, si intende     */
/*       *        al 100%                                        */
/*       *-------------------------------------------------------*/
  pib_rda                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo documento per fatturazione elettronica    */
/*       *-------------------------------------------------------*/
  tdo_fel                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
