/*    *==========================================================*/
/*    * Tabella [sff] Fatture fornitori da scadenziario    {scf} */
/*    *                                                          */
/*    * Aggiornamento 09.11.2021                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_ftf),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_ftf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di registrazione fattura */
/*       * fornitore                                             */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento fornitore                              */
/*       *                                                       */
/*       * - 01 : Fattura                                        */
/*       * - 02 : Nota di Addebito                               */
/*       * - 03 : Nota di Accredito                              */
/*       *-------------------------------------------------------*/
  tip_ftf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo provenienza documento fornitore                  */
/*       *                                                       */
/*       * - 01 : Nazionale                                      */
/*       * - 02 : CEE                                            */
/*       * - 03 : Estero                                         */
/*       * - 04 : Dogana                                         */
/*       * - 05 : CEE - Servizi                                  */
/*       * - 06 : Estero - Servizi                               */
/*       *-------------------------------------------------------*/
  tpv_ftf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice fornitore                                      */
/*       *-------------------------------------------------------*/
  cod_fnt                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza del fornitore                       */
/*       *-------------------------------------------------------*/
  dpz_fnt                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento del fornitore                          */
/*       *-------------------------------------------------------*/
  ddo_ftf                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento del fornitore                        */
/*       *-------------------------------------------------------*/
  ndo_ftf                   varchar (20)                not null,
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
/*       * Importo fattura fornitore in valuta                   */
/*       *-------------------------------------------------------*/
  iiv_ftf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo fattura fornitore                             */
/*       *-------------------------------------------------------*/
  imp_ftf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamenti al fornitore                        */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale                               */
/*       * - 03 : alla filiale                                   */
/*       *-------------------------------------------------------*/
  inl_pgt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contro-     */
/*       * partita acquisti                                      */
/*       *                                                       */
/*       * - 0000000 : A seconda delle righe                     */
/*       * - nnnnnnn : Conto di contropartita                    */
/*       *-------------------------------------------------------*/
  ctp_acq                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Forma di pagamento per il documento                   */
/*       *-------------------------------------------------------*/
  cod_fop                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca di appoggio per pagamenti tipo:   */
/*       *                                                       */
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
/*       * Codice banca estera del fornitore                     */
/*       *-------------------------------------------------------*/
  cod_bef                   integer (05)                not null,
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
/*       * Descrizione causale contabile                         */
/*       *-------------------------------------------------------*/
  des_cau                   varchar (200)               not null,
/*       *-------------------------------------------------------*/
/*       * Numero di giornale iva per il documento               */
/*       *-------------------------------------------------------*/
  num_giv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo iva                                 */
/*       *-------------------------------------------------------*/
  npt_iva                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Totale documento, in valuta                           */
/*       *-------------------------------------------------------*/
  iva_tdo                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione per movimento di contabilita' ge-  */
/*       * nerale relativo al documento                          */
/*       *-------------------------------------------------------*/
  dat_mgd                   date                        not null,
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
/*       * Numero scadenza della prima scadenza generata per la  */
/*       * gestione del portafoglio effetti passivi              */
/*       *                                                       */
/*       * N.B. : Le scadenze successive alla prima hanno un nu- */
/*       *        mero scadenza in progressione di uno in uno    */
/*       *-------------------------------------------------------*/
  nps_scf                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di scadenze generate per la gestione del por-  */
/*       * tafoglio effetti attivi : 00..96                      */
/*       *-------------------------------------------------------*/
  ctr_scf                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di registrazione fattura  */
/*       * fornitori in gestione scadenze fornitori              */
/*       *-------------------------------------------------------*/
  prt_ffo                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no documento relativo alla ritenuta d'acconto      */
/*       *-------------------------------------------------------*/
  snx_rda                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di ritenuta d'acconto     */
/*       *-------------------------------------------------------*/
  prt_rda                   bigint  (11)                not null,
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
/*       * Flag di regime fiscale particolare                    */
/*       *                                                       */
/*       * - Spaces o N : Normale regime fiscale                 */
/*       * - S          : Regime di Scissione Pagamenti          */
/*       * - R          : Regime di Reverse Charge               */
/*       * - E          : Esclusione da dichiarazione annuale    */
/*       *-------------------------------------------------------*/
  flg_rfp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di presenza documento in formato elettronico     */
/*       *                                                       */
/*       * - Spaces o N : No                                     */
/*       * - S          : Si, documento elettronico presente     */
/*       * - A          : Si, documento elettronico archiviato   */
/*       *-------------------------------------------------------*/
  flg_sdi                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di riferimento per movimento di contabilita' ge- */
/*       * nerale relativo al documento                          */
/*       *-------------------------------------------------------*/
  dri_mgd                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero di riferimento per movimento di contabilita'   */
/*       * generale relativo al documento                        */
/*       *-------------------------------------------------------*/
  nri_mgd                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di Merce o Servizi                               */
/*       *-------------------------------------------------------*/
  flg_mos                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

