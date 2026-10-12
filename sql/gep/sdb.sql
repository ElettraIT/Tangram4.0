/*    *==========================================================*/
/*    * Tabella [sdb] Scadenze in portafoglio              {gep} */
/*    *                                                          */
/*    * Aggiornamento 22.10.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (num_sdb),
/* */
  KEY                       DATCHS                      (dat_chs,
                                                         num_sdb),
/*       *=======================================================*/
/*       * Numero scadenza : s.aa.nnnnnnnn                       */
/*       *-------------------------------------------------------*/
  num_sdb                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di emissione o cessione  */
/*       * da debitore                                           */
/*       *-------------------------------------------------------*/
  dtr_emi                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di emissione o cessione da debitore     */
/*       *-------------------------------------------------------*/
  drc_emi                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di emissione o cessione da debitore     */
/*       *-------------------------------------------------------*/
  npc_emi                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di fatture clienti        */
/*       *                                                       */
/*       * N.B.: Solo se scadenza automaticamente generata dal-  */
/*       *       la procedura di fatturazione                    */
/*       *-------------------------------------------------------*/
  prt_fcl                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero scadenza di origine, solo se la scadenza ha a- */
/*       * vuto origine da un'altra scadenza                     */
/*       *-------------------------------------------------------*/
  nsc_org                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente cui la scadenza si riferisce            */
/*       *-------------------------------------------------------*/
  cod_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo scadenza                                         */
/*       *                                                       */
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
  tip_sdb                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No dilazione concordata                            */
/*       *                                                       */
/*       * N.B.: Campo gestito solo se tipo scadenza 01          */
/*       *-------------------------------------------------------*/
  snx_dlc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo acquisizione scadenza                            */
/*       *                                                       */
/*       * - 01 : Emissione interna                              */
/*       * - 02 : Cessione da debitore                           */
/*       *-------------------------------------------------------*/
  tac_sdb                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo valuta per la scadenza                           */
/*       *                                                       */
/*       * - 01 : Scadenza in valuta base                        */
/*       * - 02 : Scadenza in valuta diversa da quella base      */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito, e forzato al    */
/*       *        valore 01                                      */
/*       *-------------------------------------------------------*/
  tvl_sdb                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di scadenza                                      */
/*       *                                                       */
/*       * N.B. : per le scadenze 'a vista' la data e' a zero    */
/*       *-------------------------------------------------------*/
  dat_sca                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo debitore                                         */
/*       *                                                       */
/*       * - 01 : Cliente                                        */
/*       *-------------------------------------------------------*/
  tip_dbt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice debitore                                       */
/*       *-------------------------------------------------------*/
  cod_dbt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza del debitore                        */
/*       *-------------------------------------------------------*/
  dpz_dbt                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro scadenza al debitore                          */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla dipendenza                                */
/*       *-------------------------------------------------------*/
  inl_sdb                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per la riscossione                             */
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
  iiv_sdb                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo scadenza in valuta base                       */
/*       *-------------------------------------------------------*/
  imp_sdb                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento di riferimento                         */
/*       *-------------------------------------------------------*/
  dat_ddr                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento di riferimento                       */
/*       *-------------------------------------------------------*/
  num_ddr                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento documento di riferimento               */
/*       *-------------------------------------------------------*/
  tmo_ddr                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento di riferimento                         */
/*       *                                                       */
/*       * - 01 : Fattura                                        */
/*       * - 02 : Nota di addebito                               */
/*       * - 11 : Nota di accredito                              */
/*       * - 21 : Anticipo                                       */
/*       * - 99 : Non classificato                               */
/*       *-------------------------------------------------------*/
  tip_ddr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo documento di riferimento, in valuta base o in */
/*       * valuta diversa, a seconda del tipo valuta per la sca- */
/*       * denza                                                 */
/*       *                                                       */
/*       * N.B.: attualmente in valuta base                      */
/*       *-------------------------------------------------------*/
  imp_ddr                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero rata documento di riferimento                  */
/*       *-------------------------------------------------------*/
  nra_ddr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI per l'appoggio in presentazione            */
/*       *-------------------------------------------------------*/
  abi_app                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CAB per l'appoggio in presentazione            */
/*       *-------------------------------------------------------*/
  cab_app                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente per l'appoggio in presentazione */
/*       * per scadenze di tipo R.I.D.                           */
/*       *-------------------------------------------------------*/
  ccc_app                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca per il pagamento tramite bonifico */
/*       * o codice nostro conto corrente postale per il paga-   */
/*       * mento per conto corrente postale                      */
/*       *-------------------------------------------------------*/
  cod_cbp                   varchar (10)                not null,
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
/*       * Segnale di si/no emissione nuova scadenza a fronte    */
/*       * dello storno                                          */
/*       *                                                       */
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
/*       * Data operazione di riscossione                        */
/*       *-------------------------------------------------------*/
  dtr_ris                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di riscossione                              */
/*       *                                                       */
/*       * - 01 : Riscossione per Contanti                       */
/*       * - 02 : Riscossione con Assegno                        */
/*       * - 03 : Riscossione per Bonifico bancario              */
/*       * - 04 : Riscossione per C/C postale                    */
/*       * - 21 : Pagamento per Contanti                         */
/*       * - 22 : Pagamento con Assegno                          */
/*       * - 23 : Pagamento per Bonifico bancario                */
/*       * - 24 : Pagamento per C/C postale                      */
/*       * - 50 : Compensazione scadenze                         */
/*       *-------------------------------------------------------*/
  mod_ris                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero operazione di riscossione, se scadenza riscos- */
/*       * sa, nel formato : s.aa.nnnnnnnn                       */
/*       *-------------------------------------------------------*/
  num_ris                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo riscosso, in valuta base o in valuta diversa, */
/*       * a seconda del tipo valuta per la scadenza             */
/*       *-------------------------------------------------------*/
  imp_ris                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale acconto/saldo per la riscossione              */
/*       *                                                       */
/*       * - 00 : non significativo, ovvero a saldo              */
/*       * - 01 : a  saldo                                       */
/*       * - 02 : in conto                                       */
/*       *                                                       */
/*       * Nota : In caso che la riscossione sia in conto, viene */
/*       *        comunque generata una nuova scadenza a fronte  */
/*       *        della differenza. Questa scadenza sara' un     */
/*       *        clone di questa scadenza in caso di riscossio- */
/*       *        ne parziale, ed in questo caso l'unico campo   */
/*       *        diverso sara' l'importo scadenza. In caso in-  */
/*       *        vece di riscossione maggiore dell'importo do-  */
/*       *        vuto, sara' generata una rimessa diretta a ti- */
/*       *        tolo di anticipo.                              */
/*       *-------------------------------------------------------*/
  aos_ris                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero nuova scadenza per la differenza tra l'importo */
/*       * della scadenza e l'importo riscosso : s.aa.nnnnnnnn   */
/*       *-------------------------------------------------------*/
  nns_ris                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero distinta di presentazione, se scadenza inclusa */
/*       * in una distinta : s.aa.nnnnnnnn                       */
/*       *-------------------------------------------------------*/
  num_ddp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo avviso ottenuto                                  */
/*       *                                                       */
/*       * - 00 : Non significativo o ancora indeterminato       */
/*       * - 03 : Ri.Ba.                                         */
/*       * - 04 : C.d.O.                                         */
/*       * - 05 : M.Av.                                          */
/*       *-------------------------------------------------------*/
  tav_ott                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di richiamo di scadenza  */
/*       * presentata con distinta                               */
/*       *-------------------------------------------------------*/
  dtr_rsp                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di richiamo di scadenza presentata      */
/*       *-------------------------------------------------------*/
  drc_rsp                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di richiamo di scadenza presentata      */
/*       *-------------------------------------------------------*/
  npc_rsp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data contabile bancaria per il movimento di richiamo  */
/*       * di scadenza presentata                                */
/*       *-------------------------------------------------------*/
  dcb_rsp                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero contabile bancaria per il movimento di richia- */
/*       * mo di scadenza presentata                             */
/*       *-------------------------------------------------------*/
  ncb_rsp                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese addebitate dalla banca al momento del   */
/*       * richiamo di scadenza presentata                       */
/*       *-------------------------------------------------------*/
  spe_rsp                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di si/no emissione nuova scadenza a fronte    */
/*       * del richiamo                                          */
/*       *                                                       */
/*       * - 01 : Si                                             */
/*       * - 02 : No                                             */
/*       *-------------------------------------------------------*/
  ens_rsp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero nuova scadenza a fronte del movimento di ri-   */
/*       * chiamo                                                */
/*       *-------------------------------------------------------*/
  nns_rsp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo nuova scadenza a fronte del movimento di richia- */
/*       * mo                                                    */
/*       *-------------------------------------------------------*/
  tns_rsp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di accredito scadenza    */
/*       * presentata al dopo incasso                            */
/*       *-------------------------------------------------------*/
  dtr_acs                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di accredito scadenza presentata al do- */
/*       * po incasso                                            */
/*       *-------------------------------------------------------*/
  drc_acs                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di accredito scadenza presentata al do- */
/*       * po incasso                                            */
/*       *-------------------------------------------------------*/
  npc_acs                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data contabile bancaria per il movimento di accredito */
/*       * scadenza presentata al dopo incasso                   */
/*       *-------------------------------------------------------*/
  dcb_acs                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero contabile bancaria per il movimento di accre-  */
/*       * dito scadenza presentata al dopo incasso              */
/*       *-------------------------------------------------------*/
  ncb_acs                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese addebitate dalla banca al momento del-  */
/*       * l'accredito scadenza presentata al dopo incasso       */
/*       *-------------------------------------------------------*/
  spe_acs                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di notizia di buon esito */
/*       * relativa a scadenza presentata con distinta non al    */
/*       * dopo incasso                                          */
/*       *-------------------------------------------------------*/
  dtr_nbe                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di notizia di buon esito relativa a     */
/*       * scadenza presentata non al dopo incasso               */
/*       *-------------------------------------------------------*/
  drc_nbe                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di notizia di buon esito relativa a     */
/*       * scadenza presentata non al dopo incasso               */
/*       *-------------------------------------------------------*/
  npc_nbe                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data contabile bancaria per il movimento di notizia   */
/*       * di buon esito relativa a scadenza presentata non al   */
/*       * dopo incasso                                          */
/*       *-------------------------------------------------------*/
  dcb_nbe                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero contabile bancaria per il movimento di notizia */
/*       * di buon esito relativa a scadenza presentata non al   */
/*       * dopo incasso                                          */
/*       *-------------------------------------------------------*/
  ncb_nbe                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di presunto buon esito   */
/*       * relativo a scadenza presentata con distinta non al    */
/*       * dopo incasso                                          */
/*       *-------------------------------------------------------*/
  dtr_pbe                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di presunto buon esito relativa a sca-  */
/*       * denza presentata non al dopo incasso                  */
/*       *-------------------------------------------------------*/
  drc_pbe                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di presunto buon esito relativa a sca-  */
/*       * denza presentata non al dopo incasso                  */
/*       *-------------------------------------------------------*/
  npc_pbe                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di insoluto su scadenza  */
/*       * presentata con distinta                               */
/*       *-------------------------------------------------------*/
  dtr_isp                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di insoluto su scadenza presentata      */
/*       *-------------------------------------------------------*/
  drc_isp                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di insoluto su scadenza presentata      */
/*       *-------------------------------------------------------*/
  npc_isp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data contabile bancaria per il movimento di insoluto  */
/*       * su scadenza presentata                                */
/*       *-------------------------------------------------------*/
  dcb_isp                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero contabile bancaria per il movimento di insolu- */
/*       * to su scadenza presentata                             */
/*       *-------------------------------------------------------*/
  ncb_isp                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese addebitate dalla banca al momento del-  */
/*       * l'insoluto su scadenza presentata                     */
/*       *-------------------------------------------------------*/
  spe_isp                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di si/no emissione nuova scadenza a fronte    */
/*       * dell'insoluto                                         */
/*       *                                                       */
/*       * - 01 : Si                                             */
/*       * - 02 : No                                             */
/*       *-------------------------------------------------------*/
  ens_isp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero nuova scadenza a fronte del movimento di in-   */
/*       * soluto                                                */
/*       *-------------------------------------------------------*/
  nns_isp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo nuova scadenza a fronte del movimento di inso-   */
/*       * luto                                                  */
/*       *-------------------------------------------------------*/
  tns_isp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di si/no contrassegno                         */
/*       *                                                       */
/*       * - 'S' : Si                                            */
/*       * - 'N' : No                                            */
/*       *-------------------------------------------------------*/
  snx_cts                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice vettore a cui si riferisce il contrassegno     */
/*       *-------------------------------------------------------*/
  vet_cts                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero ultimo livello di sollecito in cui la scadenza */
/*       * e' entrata a far parte                                */
/*       *-------------------------------------------------------*/
  liv_slc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Date sollecito                                        */
/*       *-------------------------------------------------------*/
  dso_l01                   date                        not null,
  dso_l02                   date                        not null,
  dso_l03                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data chiusura scadenza                                */
/*       * - 9999999 : Scadenza aperta                           */
/*       * - nnnnnnn : Scadenza chiusa                           */
/*       *-------------------------------------------------------*/
  dat_chs                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag/contatore di stampa effetto eseguita             */
/*       *-------------------------------------------------------*/
  flc_stp                   tinyint (02)                not null,
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
