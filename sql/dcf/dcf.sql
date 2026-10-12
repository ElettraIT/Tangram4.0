/*    *==========================================================*/
/*    * Tabella [dcf] Fornitori commerciali                {dcf} */
/*    *                                                          */
/*    * Sedi                                                     */
/*    *                                                          */
/*    * Aggiornamento 25.07.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (cod_fnt),
/* */
  KEY                       DATSYS                      (ide_dat,
                                                         cod_fnt),
/* */
  KEY                       CODMNE                      (mne_cli,
                                                         cod_fnt),
/* */
  KEY                       PRTIVA                      (prt_iva,
                                                         cod_fnt),
/* */
  KEY                       CODFIS                      (cod_fis,
                                                         cod_fnt),
/*       *=======================================================*/
/*       * Codice fornitore, deve trovare corrispondenza in      */
/*       * anagrafica fornitori per contabilita'                 */
/*       *-------------------------------------------------------*/
  cod_fnt                   integer (07)                not null COMMENT 'Codice fornitore',
/*       *-------------------------------------------------------*/
/*       * Mnemonico                                             */
/*       *-------------------------------------------------------*/
  mne_fnt     varchar (10)  not null COMMENT 'Mnemonico'       ,
/*       *-------------------------------------------------------*/
/*       * Ragione sociale                                       */
/*       *-------------------------------------------------------*/
  rag_soc                   varchar (80)                not null COMMENT 'Ragione sociale',
/*       *-------------------------------------------------------*/
/*       * Indirizzo                                             */
/*       *-------------------------------------------------------*/
  via_dcf                   varchar (80)                not null COMMENT 'Indirizzo',
/*       *-------------------------------------------------------*/
/*       * Localita'                                             */
/*       *-------------------------------------------------------*/
  loc_dcf                   varchar (80)                not null COMMENT 'Localita',
/*       *-------------------------------------------------------*/
/*       * Localita' - CAP                                       */
/*       *-------------------------------------------------------*/
  loc_cap                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Localita' - Comune                                    */
/*       *-------------------------------------------------------*/
  loc_cmn                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Localita' - Provincia                                 */
/*       *-------------------------------------------------------*/
  loc_prv                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nazione                                        */
/*       *-------------------------------------------------------*/
  cod_naz                   varchar (03)                not null COMMENT 'Codice nazione',
/*       *-------------------------------------------------------*/
/*       * Codice comune                                         */
/*       *-------------------------------------------------------*/
  cod_cmn                   integer (05)                not null COMMENT 'Codice comune',
/*       *-------------------------------------------------------*/
/*       * Codice frazione                                       */
/*       *-------------------------------------------------------*/
  cod_fzn                   integer (03)                not null COMMENT 'Codice frazione',
/*       *-------------------------------------------------------*/
/*       * Codice localita'                                      */
/*       *-------------------------------------------------------*/
  cod_lct                   integer (03)                not null COMMENT 'Codice localita',
/*       *=======================================================*/
/*       * DATI FISCALI                                          */
/*       *-------------------------------------------------------*/
/*       * Codice esenzione Iva                                  */
/*       *-------------------------------------------------------*/
  cod_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Partita Iva                                           */
/*       *-------------------------------------------------------*/
  prt_iva     varchar (11)  not null COMMENT 'Partita Iva'     ,
/*       *-------------------------------------------------------*/
/*       * Codice Fiscale                                        */
/*       *-------------------------------------------------------*/
  cod_fis     varchar (30)  not null COMMENT 'Codice Fiscale'  ,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile                                  */
/*       *-------------------------------------------------------*/
  cod_pdc     integer (07)  not null COMMENT 'Sottoconto contabile',
/*       *-------------------------------------------------------*/
/*       * Si/no va in elenchi Iva                               */
/*       *                                                       */
/*       * - 1 : Si                                              */
/*       * - 0 : No                                              */
/*       *=======================================================*/
  snx_iva                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro documenti                                     */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale (contabile)                   */
/*       * - 03 : alla dipendenza                                */
/*       *-------------------------------------------------------*/
  inl_dcm                   tinyint (02)                not null COMMENT 'Inoltro documenti',
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamenti                                     */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale (contabile)                   */
/*       * - 03 : alla dipendenza                                */
/*       *-------------------------------------------------------*/
  inl_pgt                   tinyint (02)                not null COMMENT 'Inoltro pagamenti',
/*       *-------------------------------------------------------*/
/*       * Tipo assoggettamento iva per il fornitore             */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  tas_ivf                   tinyint (02)                not null COMMENT 'Tipo assoggettamento iva per il fornitore',
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contro-     */
/*       * partita gli acquisti dal fornitore. Se a zero si      */
/*       * assumeranno le contropartite acquisti dai prodotti    */
/*       * fatturati                                             */
/*       *-------------------------------------------------------*/
  ctp_acq                   integer (07)                not null COMMENT 'Conto di contabilita generale usato come contropartita acquisti dal fornitore',
/*       *-------------------------------------------------------*/
/*       * Codice valuta applicato dal fornitore                 */
/*       *-------------------------------------------------------*/
  cod_vlt                   varchar (03)                not null COMMENT 'Codice valuta applicato dal fornitore',
/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null COMMENT 'Codice lingua',
/*       *-------------------------------------------------------*/
/*       * Codice listino                                        */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  cod_lst                   varchar (03)                not null COMMENT 'Codice listino',
/*       *-------------------------------------------------------*/
/*       * Codice categoria sconto in riga                       */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  cat_scr                   integer (05)                not null COMMENT 'Codice categoria sconto in riga',
/*       *-------------------------------------------------------*/
/*       * % sconto in riga                                      */
/*       *-------------------------------------------------------*/
  per_sr1                   decimal (05,02)             not null COMMENT '% sconto in riga',
  per_sr2                   decimal (05,02)             not null COMMENT '% sconto in riga',
  per_sr3                   decimal (05,02)             not null COMMENT '% sconto in riga',
  per_sr4                   decimal (05,02)             not null COMMENT '% sconto in riga',
  per_sr5                   decimal (05,02)             not null COMMENT '% sconto in riga',
/*       *-------------------------------------------------------*/
/*       * Codice categoria sconto in chiusura                   */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  cat_scc                   integer (05)                not null COMMENT 'Codice categoria sconto in chiusura',
/*       *-------------------------------------------------------*/
/*       * % sconto in chiusura                                  */
/*       *-------------------------------------------------------*/
  per_scc                   decimal (05,02)             not null COMMENT '% sconto in chiusura',
/*       *-------------------------------------------------------*/
/*       * Codice forma di pagamento                             */
/*       *-------------------------------------------------------*/
  cod_fop                   integer (07)                not null COMMENT 'Codice forma di pagamento',
/*       *-------------------------------------------------------*/
/*       * Tipo esclusione mesi per le scadenze                  */
/*       *                                                       */
/*       * - 01 : Nessun mese escluso                            */
/*       * - 02 : Si sposta la scadenza di un mese               */
/*       * - 03 : Si sposta la scadenza ad un giorno fisso del   */
/*       *        mese successivo                                */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  tip_esm                   tinyint (02)                not null COMMENT 'Tipo esclusione mesi per le scadenze',
/*       *-------------------------------------------------------*/
/*       * Giorno di scadenza fisso nel mese successivo, se tipo */
/*       * esclusione mese pari a 03                             */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  ggg_alt                   tinyint (02)                not null COMMENT 'Giorno di scadenza fisso nel mese successivo',
/*       *-------------------------------------------------------*/
/*       * Mesi di esclusione per le scadenze                    */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  mmm_e01                   tinyint (02)                not null COMMENT 'Mesi di esclusione per le scadenze',
  mmm_e02                   tinyint (02)                not null COMMENT 'Mesi di esclusione per le scadenze',
/*       *-------------------------------------------------------*/
/*       * Codice addebito spese incasso                         */
/*       *-------------------------------------------------------*/
  add_spi                   varchar (03)                not null COMMENT 'Codice addebito spese incasso',
/*       *-------------------------------------------------------*/
/*       * Codice addebito spese bollo                           */
/*       *-------------------------------------------------------*/
  add_spb                   varchar (03)                not null COMMENT 'Codice addebito spese bollo',
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca di appoggio per pagamenti a mezzo */
/*       * circuiti bancari                                      */
/*       *-------------------------------------------------------*/
  nos_ban                   varchar (10)                not null COMMENT 'Codice nostra banca di appoggio per pagamenti tramite circuiti bancari',
/*       *-------------------------------------------------------*/
/*       * Codice ABI dell'istituto di credito di appoggio per   */
/*       * pagamenti da effettuarsi al fornitore a mezzo banca,  */
/*       * per effettuare bonifici bancari al fornitore          */
/*       *-------------------------------------------------------*/
  cod_abi                   integer (05)                not null COMMENT 'Codice ABI dell istituto di credito di appoggio per bonifici al fornitore',
/*       *-------------------------------------------------------*/
/*       * Codice CAB dell'istituto di credito di appoggio per   */
/*       * pagamenti da effettuarsi al fornitore a mezzo banca,  */
/*       * per effettuare bonifici bancari al fornitore          */
/*       *-------------------------------------------------------*/
  cod_cab                   integer (05)                not null COMMENT 'Codice CAB dell istituto di credito di appoggio per bonifici al fornitore',
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente nella banca del fornitore di    */
/*       * appoggio per pagamenti da effettuarsi al fornitore a  */
/*       * mezzo banca, per effettuare bonifici bancari al       */
/*       * fornitore                                             */
/*       *-------------------------------------------------------*/
  ccc_app                   varchar (20)                not null COMMENT 'Codice conto corrente nella banca del fornitore di appoggio',
/*       *-------------------------------------------------------*/
/*       * Codice IBAN                                           */
/*       *-------------------------------------------------------*/
  cod_ibn                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente postale di appoggio per pa-     */
/*       * gamenti da effettuarsi al fornitore a mezzo c/c       */
/*       * postale, per effettuare bonifici postali al fornitore */
/*       *-------------------------------------------------------*/
  ccp_app                   varchar (20)                not null COMMENT 'Codice conto corrente postale di appoggio per pagamenti al fornitore',
/*       *-------------------------------------------------------*/
/*       * Indice di priorita' per i pagamenti al fornitore      */
/*       *                                                       */
/*       * - 01 : Alta                                           */
/*       * - 02 : Media                                          */
/*       * - 03 : Bassa                                          */
/*       *-------------------------------------------------------*/
  ipr_pgt                   tinyint (02)                not null COMMENT 'Indice di priorita per i pagamenti al fornitore',
/*       *-------------------------------------------------------*/
/*       * Data di acquisizione del fornitore                    */
/*       *-------------------------------------------------------*/
  dat_aqz                   date                        not null COMMENT 'Data di acquisizione del fornitore',
/*       *-------------------------------------------------------*/
/*       * Codice zona                                           */
/*       *-------------------------------------------------------*/
  cod_zon                   integer (05)                not null COMMENT 'Codice zona',
/*       *-------------------------------------------------------*/
/*       * Codice categoria                                      */
/*       *-------------------------------------------------------*/
  cod_cat                   integer (05)                not null COMMENT 'Codice categoria',
/*       *-------------------------------------------------------*/
/*       * Codice statistico                                     */
/*       *-------------------------------------------------------*/
  cod_stt                   integer (05)                not null COMMENT 'Codice statistico',
/*       *-------------------------------------------------------*/
/*       * Classe di budget                                      */
/*       *-------------------------------------------------------*/
  cla_bdg                   integer (05)                not null COMMENT 'Classe di budget',
/*       *-------------------------------------------------------*/
/*       * Segnale di fornitore con blocco sugli ordini          */
/*       *                                                       */
/*       * - 01 : No, si possono fare ordini al fornitore        */
/*       * - 02 : Si, ordini al fornitore bloccati               */
/*       *-------------------------------------------------------*/
  for_blo                   tinyint (02)                not null COMMENT 'Segnale di fornitore con blocco sugli ordini',
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco della       */
/*       * eseguibilita' degli ordini                            */
/*       *-------------------------------------------------------*/
  dor_blo                   date                        not null COMMENT 'Data del blocco o sblocco della eseguibilita degli ordini',
/*       *-------------------------------------------------------*/
/*       * Status commerciale del fornitore                      */
/*       *                                                       */
/*       *  - 01 : Normale                                       */
/*       *  - 11 : Esauriti i rapporti commerciali               */
/*       *  - 21 : Sostituito da nostro nuovo fornitore          */
/*       *  - 51 : Cessata attivita'                             */
/*       *  - 52 : Cessata attivita', ma sostituito da nostro    */
/*       *         nuovo fornitore                               */
/*       *  - 61 : In contenzioso                                */
/*       *  - 62 : In contenzioso, ma sostituito da nostro nuovo */
/*       *         fornitore                                     */
/*       *  - 71 : Fallito                                       */
/*       *  - 72 : Fallito, ma sostituito da ns. nuovo fornitore */
/*       *-------------------------------------------------------*/
  sta_tus                   tinyint (02)                not null COMMENT 'Status commerciale del fornitore',
/*       *-------------------------------------------------------*/
/*       * Data di determinazione dello status commerciale       */
/*       *                                                       */
/*       * Solo se status diverso da 01                          */
/*       *-------------------------------------------------------*/
  sta_tud                   date                        not null COMMENT 'Data di determinazione dello status commerciale',
/*       *-------------------------------------------------------*/
/*       * Codice fornitore di riferimento in relazione allo     */
/*       * status del fornitore, ovvero codice nuovo fornitore   */
/*       *                                                       */
/*       * Solo se status a 21,52,62,72                          */
/*       *-------------------------------------------------------*/
  sta_tuc                   integer (07)                not null COMMENT 'Codice fornitore di riferimento in relazione allo status del fornitore',
/*       *-------------------------------------------------------*/
/*       * Modalita' di trattamento nelle statistiche per i      */
/*       * documenti precedenti il cambiamento di status         */
/*       *                                                       */
/*       * Solo se codice fornitore di riferimento diverso da 0  */
/*       *                                                       */
/*       *  - 00 : Non significativo                             */
/*       *  - 01 : Statistiche distinte                          */
/*       *  - 02 : Statistiche girate sul fornitore di riferi-   */
/*       *         mento ma sempre sulla sua sede principale     */
/*       *  - 03 : Statistiche girate sul fornitore di riferi-   */
/*       *         mento conservando il codice dipendenza        */
/*       *-------------------------------------------------------*/
  sta_tux                   tinyint (02)                not null COMMENT 'Modalita di trattamento nelle statistiche per i documenti precedenti il cambiamento di status',
/*       *-------------------------------------------------------*/
/*       * Codice CIN italiano                                   */
/*       *-------------------------------------------------------*/
  cin_ita                   varchar (01)                not null COMMENT 'Codice CIN italiano',
/*       *-------------------------------------------------------*/
/*       * Codice CIN europeo                                    */
/*       *-------------------------------------------------------*/
  cin_eur                   varchar (02)                not null COMMENT 'Codice CIN europeo',
/*       *-------------------------------------------------------*/
/*       * Modalita' di calcolo importi in riga documenti        */
/*       *                                                       */
/*       *  - spaces : Standard (Prezzo netto x qta')            */
/*       *  - 'L'    : Prezzo lordo x qta', scontato             */
/*       *-------------------------------------------------------*/
  tip_mci                   varchar (01)                not null COMMENT 'Modalita di calcolo importi in riga documenti',
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
