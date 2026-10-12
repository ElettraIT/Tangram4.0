/*    *==========================================================*/
/*    * Tabella [bit] Bolle clienti : testate              {bol} */
/*    *                                                          */
/*    * Aggiornamento 03.08.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (num_prt),
/* */
  KEY                       FATSNX                      (fat_num,
                                                         fat_snx,
                                                         tip_arc),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                001 */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento                                        */
/*       *-------------------------------------------------------*/
  cod_tmo                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo trattamento per le statistiche                   */
/*       *                                                       */
/*       * - A  : Acquisto                                       */
/*       * - V  : Vendita                                        */
/*       * - RV : Reso vendita                                   */
/*       * - RA : Reso acquisto                                  */
/*       * - N  : Neutro                                         */
/*       *                                                       */
/*       * N.B. : Campo proveniente da tipo movimento            */
/*       *-------------------------------------------------------*/
  tip_trs                   varchar (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo di fatturazione                     */
/*       *-------------------------------------------------------*/
  prt_fti                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo magazzino                           */
/*       *-------------------------------------------------------*/
  prt_mag                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo magazzino aggiuntivo                */
/*       *-------------------------------------------------------*/
  pr2_mag                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No movimento che interessa la fatturazione         */
/*       *                                                       */
/*       * - 01 : No                                             */
/*       * - 02 : Si, il documento deve successivamente essere   */
/*       *            sottoposta alla procedura di fatturazio-   */
/*       *            ne automatica da bolle emesse              */
/*       * - 03 : Si, il documento stesso e' anche una fattura   */
/*       *-------------------------------------------------------*/
  int_ftr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento di fatturazione                        */
/*       *                                                       */
/*       * - Se movimento che interessa la fatturazione 01       */
/*       *      - Valore fisso Spaces, non significativo         */
/*       *                                                       */
/*       * - Se documento che interessa la fatturazione 02       */
/*       *      - Codice tipo movimento di fatturazione che sa-  */
/*       *        ra' utilizzato dalla procedura di fatturazio-  */
/*       *        ne automatica da bolle emesse                  */
/*       *                                                       */
/*       * - Se movimento che interessa la fatturazione 03       */
/*       *      - Codice tipo movimento di fatturazione corri-   */
/*       *        spondente alla componente 'fattura' del pre-   */
/*       *        sente tipo movimento di bollettazione          */
/*       *-------------------------------------------------------*/
  tmo_ftr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Documento da sottoporre a fatturazione differita      */
/*       * - N : No                                              */
/*       * - S : Si                                              */
/*       *                                                       */
/*       * Nota : Per le fatture accompagnatorie questo valore   */
/*       *        e' ovviamente posto a 'N'                      */
/*       *-------------------------------------------------------*/
  fat_snx                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Data fattura                                      010 */
/*       *-------------------------------------------------------*/
  fat_dat                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero fattura : s.aa.dd.nnnnnn                       */
/*       *-------------------------------------------------------*/
  fat_num                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo bolla nella fattura                */
/*       *-------------------------------------------------------*/
  fat_npb                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No documento che accompagna la merce               */
/*       *-------------------------------------------------------*/
  snx_acm                   varchar (01)                not null,
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
/*       * - xx     : numero giornale iva, se bolla/fattura      */
/*       *            codice dipendenza  , se bolla              */
/*       * - nnnnnn : numero progressivo                         */
/*       *-------------------------------------------------------*/
  num_doc                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No fattura accompagnatoria                     017 */
/*       *                                                       */
/*       * ___ DA ELIMINARE ??? ___                              */
/*       *-------------------------------------------------------*/
  snx_fac                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Secolo ed anno relativi alla data documento           */
/*       *-------------------------------------------------------*/
  scl_ann                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla numerazione relativa al numero documento        */
/*       *                                                       */
/*       * Nota : Per le fatture accompagnatorie questo campo    */
/*       *        contiene la sigla numerazione del relativo     */
/*       *        codice tipo movimento di fatturazione (vedi    */
/*       *        'tmo_ftr')                                     */
/*       *-------------------------------------------------------*/
  sgl_num                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio per il movimento                    020 */
/*       *                                                       */
/*       * - N : No tipo archivio                                */
/*       * - C : Cliente                                         */
/*       * - F : Fornitore                                       */
/*       * - D : Deposito                                        */
/*       * - A : Agente (Incaricato alla vendita)                */
/*       *                                                       */
/*       * N.B. : Se il documento interessa la fatturazione, il  */
/*       *        tipo archivio deve essere pari a 'C'           */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza archivio                            */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo indirizzo di spedizione                          */
/*       *                                                       */
/*       * - 01 : alla sede legale                               */
/*       * - 02 : manuale                                        */
/*       * - 03 : alla sede                                      */
/*       * - 04 : alla dipendenza                                */
/*       *-------------------------------------------------------*/
  tip_ids                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua per il documento                        */
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
/*       *-------------------------------------------------------*/
  ass_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contro- 030 */
/*       * partita vendite                                       */
/*       *                                                       */
/*       * - 0000000 : A seconda delle righe                     */
/*       * - nnnnnnn : Conto di contropartita                    */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Fatturazione separata per questo documento        031 */
/*       *                                                       */
/*       * - Spaces : Non significativo, perche' documento che   */
/*       *            non interessa la fatturazione              */
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
/*       * Inoltro documenti                                     */
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
/*       * Inoltro pagamenti                                     */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla filiale                                   */
/*       *-------------------------------------------------------*/
  inl_pgt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice listino da applicare per il documento          */
/*       *                                                       */
/*       * - Spaces : listino base                               */
/*       * - xxx    : listino xxx                                */
/*       *-------------------------------------------------------*/
  cod_lst                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in riga associata al cliente     035 */
/*       *-------------------------------------------------------*/
  csr_aac                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto in riga associate al cliente                 */
/*       *-------------------------------------------------------*/
  psr_ac1                   decimal (05,02)             not null,
  psr_ac2                   decimal (05,02)             not null,
  psr_ac3                   decimal (05,02)             not null,
  psr_ac4                   decimal (05,02)             not null,
  psr_ac5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in chiusura associata al cliente 041 */
/*       *-------------------------------------------------------*/
  csc_aac                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto chiusura associato al cliente                */
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
/*       * di appoggio per gli effetti, per il documento     050 */
/*       *-------------------------------------------------------*/
  cod_cab                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente del cliente presso l'istituto   */
/*       * di credito per l'appoggio del pagamento, solo per gli */
/*       * accrediti pre-autorizzati in conto 'RID', per il do-  */
/*       * cumento.                                              */
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
/*       * Codice agente per le provvigioni sul documento    060 */
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
/*       * N.B.: Campo attualmente non gestito               070 */
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
/*       * Importo sconto in chiusura, in valuta             080 */
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
/*       * Importo spese incasso calcolate, valuta           090 */
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
/*       * TRASPORTO                                             */
/*       *-------------------------------------------------------*/
/*       *-------------------------------------------------------*/
/*       * Trasporto a cura                                  095 */
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
/*       * Peso totale                                           */
/*       *                                                       */
/*       * N.B. : informazione attualmente non obbligatoria      */
/*       *-------------------------------------------------------*/
  pes_tot                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Data inizio trasporto                             100 */
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
/*       * Codice primo vettore                                  */
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
/*       * Codice annotazioni                                    */
/*       *-------------------------------------------------------*/
  cod_ant                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * TRASPORTO - fine                                      */
/*       *-------------------------------------------------------*/
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
/*       *                                                       */
/*       *        PROBABILMENTE DA ELEMINARE IN QUANTO RELATIVO  */
/*       *        ALLE FATTURE ACCOMPAGNATORIE                   */
/*       *                                                   110 */
/*       *-------------------------------------------------------*/
  nps_sdb                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di scadenze generate per la gestione del por-  */
/*       * tafoglio effetti attivi : 00..96                      */
/*       *                                                       */
/*       *        PROBABILMENTE DA ELEMINARE IN QUANTO RELATIVO  */
/*       *        ALLE FATTURE ACCOMPAGNATORIE                   */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  ctr_sdb                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Totale documento, in valuta                           */
/*       *-------------------------------------------------------*/
  iva_tdo                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Totale riscosso alla consegna                         */
/*       *-------------------------------------------------------*/
  tot_rsc                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di riscossione alla consegna                     */
/*       *-------------------------------------------------------*/
  dat_rsc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero di stampe eseguite, max 99                     */
/*       *                                                       */
/*       * ___ DA ELIMINARE ___                                  */
/*       *-------------------------------------------------------*/
  ctr_stp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di bolla completamente chiusa                116 */
/*       *                                                       */
/*       * - Spaces : Non ancora chiusa                          */
/*       * - #      : Completamente chiusa                       */
/*       *-------------------------------------------------------*/
  flg_chs                   varchar (01)                not null,
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
/*       * 01 : Flag di esclusione provvisoria bolla dalla       */
/*       *      fatturazione                                     */
/*       *       - Spaces : no esclusione                        */
/*       *       - '#'    : esclusione                           */
/*       *                                                       */
/*       *      N.B.: Soggetto a personalizzazione               */
/*       *                                                       */
/*       * 02 : (libero)                                         */
/*       *                                                       */
/*       * 03 : Flag di bolla effettivamente consegnata          */
/*       *       - Spaces : non consegnata                       */
/*       *       - '#'    : consegnata                           */
/*       *-------------------------------------------------------*/
  flg_nb1                   varchar (01)                not null,
  flg_nb2                   varchar (01)                not null,
  flg_nb3                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia effettuabile                     127 */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
