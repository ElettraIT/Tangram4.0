/*    *==========================================================*/
/*    * Tabella [oct] Ordini fornitori : testate           {orf} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
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
/*       * Codice ns. dipendenza                                 */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento : saa.dd.nnnnnn                      */
/*       *                                                       */
/*       * - s      : secolo                                     */
/*       * - aa     : anno                                       */
/*       * - dd     : codice dipendenza                          */
/*       * - nnnnnn : numero progressivo                         */
/*       *-------------------------------------------------------*/
  num_doc                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Secolo ed anno della data documento                   */
/*       *-------------------------------------------------------*/
  scl_ann                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla numerazione                                     */
/*       *-------------------------------------------------------*/
  sgl_num                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio per il movimento                        */
/*       *                                                       */
/*       * - F : Fornitore                                       */
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
/*       * Codice archivio per la fatturazione                   */
/*       *-------------------------------------------------------*/
  arc_plf                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Dipendenza archivio per la fatturazione               */
/*       *-------------------------------------------------------*/
  dpz_plf                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo indirizzo di spedizione                          */
/*       *                                                       */
/*       * - 01 : Alla nostra dipendenza di emissione            */
/*       * - 02 : Manuale                                        */
/*       * - 03 : Ad una nostra dipendenza                       */
/*       *-------------------------------------------------------*/
  tip_ids                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ns. dipendenza di spedizione                   */
/*       *-------------------------------------------------------*/
  dpz_ids                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data conferma ordine del fornitore                    */
/*       *-------------------------------------------------------*/
  cof_dat                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero conferma ordine del fornitore                  */
/*       *-------------------------------------------------------*/
  cof_num                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Riferimenti conferma ordine del fornitore             */
/*       *-------------------------------------------------------*/
  cof_rif                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice responsabile ordine                            */
/*       *-------------------------------------------------------*/
  cod_rsp                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data consegna prevista di massima                     */
/*       *-------------------------------------------------------*/
  dat_cns                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Formato di stampa data consegna                       */
/*       *                                                       */
/*       * - 01 : Formato gg/mm/aa                               */
/*       * - 02 : Formato mm/aa                                  */
/*       * - 03 : No stampa                                      */
/*       * - 04 : Formato st/aaaa                                */
/*       *-------------------------------------------------------*/
  fds_dtc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di evasione richiesta                            */
/*       *                                                       */
/*       * - 01 : Il totale ordine                               */
/*       * - 02 : Il totale riga                                 */
/*       * - 03 : Il parziale riga                               */
/*       *-------------------------------------------------------*/
  tip_eva                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Priorita' di evasione                                 */
/*       *-------------------------------------------------------*/
  pri_eva                   varchar (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice condizioni di acquisto                         */
/*       *-------------------------------------------------------*/
  cod_cda                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Commento ad uso interno                               */
/*       *-------------------------------------------------------*/
  com_int                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
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
/*       * Conto di contabilita' generale usato come contro-     */
/*       * partita acquisti                                      */
/*       *                                                       */
/*       * - 0000000 : A seconda delle righe                     */
/*       * - nnnnnnn : Conto di contropartita                    */
/*       *-------------------------------------------------------*/
  ctp_acq                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro documento                                     */
/*       *                                                       */
/*       * - 01 : Alla sede                                      */
/*       * - 02 : Alla sede legale                               */
/*       * - 03 : Alla dipendenza del cliente                    */
/*       *-------------------------------------------------------*/
  inl_dcm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamenti                                     */
/*       *                                                       */
/*       * - 01 : Alla sede                                      */
/*       * - 02 : Alla sede legale                               */
/*       * - 03 : Alla dipendenza del cliente                    */
/*       *-------------------------------------------------------*/
  inl_pgt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice listino da applicare                           */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  cod_lst                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in riga associata al fornitore       */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  csr_aaf                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto in riga associata al fornitore               */
/*       *-------------------------------------------------------*/
  psr_af1                   decimal (05,02)             not null,
  psr_af2                   decimal (05,02)             not null,
  psr_af3                   decimal (05,02)             not null,
  psr_af4                   decimal (05,02)             not null,
  psr_af5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Categoria sconto in chiusura associata al fornitore   */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  csc_aaf                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto chiusura associato al fornitore              */
/*       *-------------------------------------------------------*/
  psc_aaf                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Forma di pagamento per il documento                   */
/*       *-------------------------------------------------------*/
  cod_fop                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sconto pagamento associato alla forma di pagamento    */
/*       *-------------------------------------------------------*/
  scp_aap                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca di appoggio per pagamenti da ef-  */
/*       * fettuarsi al fornitore a mezzo circuiti bancari       */
/*       *-------------------------------------------------------*/
  nos_ban                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI dell'istituto di credito di appoggio per   */
/*       * pagamenti da effettuarsi al fornitore a mezzo banca,  */
/*       * tramite bonifico                                      */
/*       *-------------------------------------------------------*/
  cod_abi                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice sportello CAB per l'istituto di credito di ap- */
/*       * poggio per pagamenti da effettuarsi al fornitore a    */
/*       * mezzo banca, tramite bonifico                         */
/*       *-------------------------------------------------------*/
  cod_cab                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente nello sportello dell'istituto   */
/*       * di credito di appoggio per pagamenti da effettuarsi   */
/*       * al fornitore a mezzo banca, tramite bonifico          */
/*       *-------------------------------------------------------*/
  ccc_app                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente postale di appoggio per paga-   */
/*       * menti da effettuarsi al fornitore a mezzo c/c posta-  */
/*       * le, per bonifici postali al fornitore                 */
/*       *-------------------------------------------------------*/
  ccp_app                   varchar (20)                not null,
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
/*       * Importo sconto pagamento, in valuta                   */
/*       *-------------------------------------------------------*/
  tot_scp                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto sconto pagamento                             */
/*       *-------------------------------------------------------*/
  per_scp                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Totale documento, in valuta                           */
/*       *-------------------------------------------------------*/
  tot_doc                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di stampe eseguite, max 99                     */
/*       *-------------------------------------------------------*/
  ctr_stp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di ordine completamente chiuso                   */
/*       *                                                       */
/*       * - Spaces : Non ancora chiuso                          */
/*       * - #      : Completamente chiuso                       */
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
 
