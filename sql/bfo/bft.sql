/*    *==========================================================*/
/*    * Tabella [bft] Bolle fornitori : testate            {bfo} */
/*    *                                                          */
/*    * Aggiornamento 30.05.2026                                 */
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
/*       * Numero protocollo fattura fornitori                   */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito, in quanto non   */
/*       *        sono gestite le fatture accompagnatorie        */
/*       *-------------------------------------------------------*/
  prt_ftf                   bigint  (11)                not null,
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
/*       * - 02 : Si, il documento sara successivamente          */
/*       *            sottoposto a fatturazione dal fornitore    */
/*       * - 03 : Si, il documento stesso e' una fattura         */
/*       *                                                       */
/*       * N.B. : Il valore 03 non e' attualmente ammesso, in    */
/*       *        quanto non sono gestite le fatture             */
/*       *        accompagnatorie                                */
/*       *-------------------------------------------------------*/
  int_ftr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento di fatturazione                        */
/*       *                                                       */
/*       * - Se interessa la fatturazione 01                     */
/*       *      - Valore fisso Spaces, non significativo         */
/*       *                                                       */
/*       * - Se interessa la fatturazione 02                     */
/*       *      - Valore fisso Spaces, non significativo         */
/*       *                                                       */
/*       * - Se interessa la fatturazione 03                     */
/*       *      - Codice tipo movimento fatturazione             */
/*       *        corrispondente alla componente 'fattura' del   */
/*       *        presente tipo movimento                        */
/*       *                                                       */
/*       * N.B. : Attualmente il valore di questo campo sara'    */
/*       *        sempre a spaces, in quanto non sono gestite le */
/*       *        fatture accompagnatorie per i fornitori        */
/*       *-------------------------------------------------------*/
  tmo_ftr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra dipendenza che ha ricevuto il documento */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione                                    */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio per il movimento                        */
/*       *                                                       */
/*       * - N : No tipo archivio                                */
/*       * - C : Cliente                                         */
/*       * - F : Fornitore                                       */
/*       * - D : Deposito                                        */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza                                     */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio per la fatturazione                   */
/*       *-------------------------------------------------------*/
  arc_plf                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza archivio per la fatturazione        */
/*       *-------------------------------------------------------*/
  dpz_plf                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo indirizzo di spedizione                          */
/*       *                                                       */
/*       * - 01 : alla sede legale                               */
/*       * - 02 : manuale                                        */
/*       * - 03 : alla sede                                      */
/*       * - 04 : alla dipendenza                                */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  tip_ids                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento del fornitore                          */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento del fornitore                        */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (20)                not null,
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
/*       * Conto di contabilita' generale usato come contro-     */
/*       * partita acquisti                                      */
/*       *                                                       */
/*       * - 0000000 : A seconda delle righe                     */
/*       * - nnnnnnn : Conto di contropartita                    */
/*       *-------------------------------------------------------*/
  ctp_acq                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamenti al fornitore                        */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla filiale                                   */
/*       *-------------------------------------------------------*/
  inl_pgt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice listino applicato dal fornitore                */
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
/*       * % sconto in riga associate al fornitore               */
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
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  prt_mgd                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero registrazioni di contabilita' generale genera- */
/*       * te per il documento                                   */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  nrg_mgd                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di riferimento per movimento di contabilita' ge- */
/*       * nerale relativo al documento                          */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  dri_mgd                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero di riferimento per movimento di contabilita'   */
/*       * generale relativo al documento                        */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  nri_mgd                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero scadenza della prima scadenza generata per la  */
/*       * gestione del portafoglio effetti passivi              */
/*       *                                                       */
/*       * N.B. : Le scadenze successive alla prima hanno un nu- */
/*       *        mero scadenza in progressione di uno in uno    */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  nps_scf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di scadenze generate per la gestione del por-  */
/*       * tafoglio effetti attivi : 00..96                      */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  ctr_scf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di bolla completamente chiusa                    */
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
 