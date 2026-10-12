/*    *==========================================================*/
/*    * Tabella [dcc] Clienti commerciali                  {dcc} */
/*    *                                                          */
/*    * Sedi                                                     */
/*    *                                                          */
/*    * Aggiornamento 04.05.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (cod_cli),
/* */
  KEY                       DATSYS                      (ide_dat,
                                                         cod_cli),
/* */
  KEY                       CODMNE                      (mne_cli,
                                                         cod_cli),
/* */
  KEY                       PRTIVA                      (prt_iva,
                                                         cod_cli),
/* */
  KEY                       CODFIS                      (cod_fis,
                                                         cod_cli),
/*       *=======================================================*/
/*       * Codice cliente, deve trovare corrispondenza in        */
/*       * anagrafica clienti per contabilita'                   */
/*       *-------------------------------------------------------*/
  cod_cli     integer (09)  not null COMMENT 'Codice'          ,
/*       *-------------------------------------------------------*/
/*       * Mnemonico                                             */
/*       *-------------------------------------------------------*/
  mne_cli     varchar (10)  not null COMMENT 'Mnemonico'       ,
/*       *-------------------------------------------------------*/
/*       * Ragione sociale                                       */
/*       *-------------------------------------------------------*/
  rag_soc     varchar (80)  not null COMMENT 'Ragione sociale' ,
/*       *-------------------------------------------------------*/
/*       * Indirizzo                                             */
/*       *-------------------------------------------------------*/
  via_dcc     varchar (80)  not null COMMENT 'Indirizzo'       ,
/*       *-------------------------------------------------------*/
/*       * Localita'                                             */
/*       *-------------------------------------------------------*/
  loc_dcc     varchar (80)  not null COMMENT 'Localita'        ,
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
  cod_naz     varchar (03)  not null COMMENT 'Codice nazione'  ,
/*       *-------------------------------------------------------*/
/*       * Codice comune                                         */
/*       *-------------------------------------------------------*/
  cod_cmn                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice frazione                                       */
/*       *-------------------------------------------------------*/
  cod_fzn                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice localita'                                      */
/*       *-------------------------------------------------------*/
  cod_lct                   integer (03)                not null,
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
  inl_dcm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Inoltro pagamenti                                     */
/*       *                                                       */
/*       * - 01 : alla sede                                      */
/*       * - 02 : alla sede legale (contabile)                   */
/*       * - 03 : alla dipendenza                                */
/*       *-------------------------------------------------------*/
  inl_pgt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di stampa della fattura                     */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 10 : Normale                                        */
/*       * - 20 : Riepilogata per prodotti venduti               */
/*       *-------------------------------------------------------*/
  mof_sft                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo assoggettamento iva per il cliente               */
/*       *                                                       */
/*       * - 01 : Sempre soggetto ad iva                         */
/*       * - 02 : Sempre esente da iva, secondo il codice di     */
/*       *        esenzione espresso nell'anagrafica cliente     */
/*       *        di contabilita' generale                       */
/*       * - 03 : Cliente con plafond, quindi soggetto oppure    */
/*       *        esente a seconda della fornitura, fermo        */
/*       *        restando che in caso di esenzione il codice    */
/*       *        di esenzione e' quello di cui sopra            */
/*       *-------------------------------------------------------*/
  tas_ivc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contro-     */
/*       * partita per le vendite al cliente. Se a zero si       */
/*       * assumeranno le contropartite vendite dai prodotti     */
/*       * fatturati                                             */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento preferenziale per la consegna          */
/*       *                                                       */
/*       * - 01 : Nessuna preferenza                             */
/*       * - 11 : Bolla di consegna                              */
/*       * - 21 : Fattura accompagnatoria                        */
/*       *                                                       */
/*       * N.B. : campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  tdp_plc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Periodicita' di fatturazione automatica               */
/*       *                                                       */
/*       * - 01 : giornaliera                                    */
/*       * - 02 : settimanale                                    */
/*       * - 03 : decadale                                       */
/*       * - 04 : quindicinale                                   */
/*       * - 05 : mensile                                        */
/*       * - 06 : bimestrale                                     */
/*       * - 07 : trimestrale                                    */
/*       * - 08 : quadrimestrale                                 */
/*       * - 09 : semestrale                                     */
/*       * - 10 : annuale                                        */
/*       * - 11 : biennale                                       */
/*       *-------------------------------------------------------*/
  per_fat                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo raggruppamento bolle in fattura per il cliente   */
/*       *                                                       */
/*       * - 01 : Una unica fattura per tutte le bolle e per     */
/*       *        qualsiasi destinatario del cliente             */
/*       * - 02 : Una fattura per ogni destinatario del cliente  */
/*       *        ente, ma raggruppando le bolle                 */
/*       * - 03 : Una fattura per ogni bolla                     */
/*       * - 11 : Una fattura per ogni ordine                    */
/*       *-------------------------------------------------------*/
  rag_bft                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo esposizione prezzi e sconti in riga documento    */
/*       *                                                       */
/*       *  - 00 : Non significativo                             */
/*       *  - 01 : Standard, come definito in personalizzazione  */
/*       *  - 11 : Prezzo di vendita e sconti                    */
/*       *  - 21 : Solo prezzo netto                             */
/*       *-------------------------------------------------------*/
  epz_pes                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice valuta da applicare al cliente                 */
/*       *-------------------------------------------------------*/
  cod_vlt                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Momento di applicazione del cambio valuta             */
/*       *                                                       */
/*       *  - 00 : Non significativo                             */
/*       *  - 01 : Standard, come definito in personalizzazione  */
/*       *         zione                                         */
/*       *  - 11 : Al momento della bolla                        */
/*       *  - 21 : Al momento della fatturazione                 */
/*       *-------------------------------------------------------*/
  mom_acv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No rapporti con legame valutario                   */
/*       *                                                       */
/*       *  - 0 : Non significativo                              */
/*       *  - 1 : Si                                             */
/*       *-------------------------------------------------------*/
  snx_rlv                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Momento di applicazione del legame valutario          */
/*       *                                                       */
/*       *  - 00 : Non significativo, in quanto si/no rapporti   */
/*       *         in legame valutario diverso da Si             */
/*       *  - 01 : Standard, come definito in personalizzazione  */
/*       *         zione                                         */
/*       *  - 11 : Al momento della bolla                        */
/*       *  - 21 : Al momento della fatturazione                 */
/*       *-------------------------------------------------------*/
  mom_alv                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua                                         */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di calcolo importo                          */
/*       *                                                       */
/*       *  - spaces : Standard (Prezzo netto x qta')            */
/*       *  - 'L'    : Prezzo lordo x qta', scontato             */
/*       *                                                       */
/*       * N.B. : Se codice dipendenza diverso da spaces, il     */
/*       *        valore non e' significativo, in quanto va-     */
/*       *        le quello espresso in anagrafica commer-       */
/*       *        ciale del cliente principale.                  */
/*       *                                                       */
/*       * N.B. : campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  tip_mci                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice listino                                        */
/*       *-------------------------------------------------------*/
  cod_lst                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria sconto in riga                       */
/*       *-------------------------------------------------------*/
  cat_scr                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto in riga                                      */
/*       *-------------------------------------------------------*/
  per_sr1                   decimal (05,02)             not null,
  per_sr2                   decimal (05,02)             not null,
  per_sr3                   decimal (05,02)             not null,
  per_sr4                   decimal (05,02)             not null,
  per_sr5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria sconto in chiusura                   */
/*       *-------------------------------------------------------*/
  cat_scc                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % sconto in chiusura                                  */
/*       *-------------------------------------------------------*/
  per_scc                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice forma di pagamento                             */
/*       *-------------------------------------------------------*/
  cod_fop                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo esclusione mesi per le scadenze                  */
/*       *                                                       */
/*       * - 01 : Nessun mese escluso                            */
/*       * - 02 : Si sposta la scadenza di un mese               */
/*       * - 03 : Si sposta la scadenza ad un giorno fisso del   */
/*       *        mese successivo                                */
/*       *-------------------------------------------------------*/
  tip_esm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Giorno di scadenza fisso nel mese successivo, se tipo */
/*       * esclusione mese pari a 03                             */
/*       *-------------------------------------------------------*/
  ggg_alt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Mesi di esclusione per le scadenze                    */
/*       *-------------------------------------------------------*/
  mmm_e01                   tinyint (02)                not null,
  mmm_e02                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice addebito spese incasso                         */
/*       *-------------------------------------------------------*/
  add_spi                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice addebito spese bollo                           */
/*       *-------------------------------------------------------*/
  add_spb                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ABI dell'istituto di credito di appoggio per   */
/*       * gli effetti                                           */
/*       *-------------------------------------------------------*/
  cod_abi                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice CAB dell'istituto di credito di appoggio per   */
/*       * gli effetti                                           */
/*       *-------------------------------------------------------*/
  cod_cab                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice conto corrente nella banca del cliente, solo   */
/*       * per accrediti pre-autorizzati in conto                */
/*       *-------------------------------------------------------*/
  ccc_app                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice IBAN                                           */
/*       *-------------------------------------------------------*/
  cod_ibn                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca per pagamenti per mezzo bonifico  */
/*       * bancario                                              */
/*       *-------------------------------------------------------*/
  nos_ban                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca presentazione effetti relativi al */
/*       * cliente                                               */
/*       *-------------------------------------------------------*/
  nos_bpe                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostro c/c postale per pagamenti per mezzo c/c */
/*       * postale                                               */
/*       *-------------------------------------------------------*/
  nos_ccp                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Scadenze da raggruppare per il cliente, prima della   */
/*       * composizione distinta                                 */
/*       *                                                       */
/*       * - 01 : No                                             */
/*       * - 02 : Si, il cliente le scadenze raggruppate         */
/*       *-------------------------------------------------------*/
  scd_rgr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente                                         */
/*       *-------------------------------------------------------*/
  cod_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice sub-agente                                     */
/*       *-------------------------------------------------------*/
  sub_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria provvigionale                        */
/*       *-------------------------------------------------------*/
  cat_pvg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % provvigioni                                         */
/*       *-------------------------------------------------------*/
  per_pv1                   decimal (05,02)             not null,
  per_pv2                   decimal (05,02)             not null,
  per_pv3                   decimal (05,02)             not null,
/*       *=======================================================*/
/*       * Codice zona                                           */
/*       *-------------------------------------------------------*/
  cod_zon                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria                                      */
/*       *-------------------------------------------------------*/
  cod_cat                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice statistico                                     */
/*       *-------------------------------------------------------*/
  cod_stt                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di acquisizione del cliente                      */
/*       *=======================================================*/
  dat_aqz                   date                        not null,
/*       *=======================================================*/
/*       * Status commerciale del cliente                        */
/*       *                                                       */
/*       *  - 01 : Normale                                       */
/*       *  - 09 : Normale, ma non entra nelle statistiche       */
/*       *         di vendita sul fatturato                      */
/*       *                                                       */
/*       *  - 11 : Esauriti i rapporti commerciali               */
/*       *                                                       */
/*       *  - 21 : Sostituito da nostro nuovo cliente            */
/*       *                                                       */
/*       *  - 51 : Cessata attivita'                             */
/*       *  - 52 : Cessata attivita', ma sostituito da nostro    */
/*       *         nuovo cliente                                 */
/*       *                                                       */
/*       *  - 61 : In contenzioso                                */
/*       *  - 62 : In contenzioso, ma sostituito da nostro nuovo */
/*       *         cliente                                       */
/*       *                                                       */
/*       *  - 71 : Fallito                                       */
/*       *  - 72 : Fallito, ma sostituito da ns. nuovo cliente   */
/*       *-------------------------------------------------------*/
  sta_tus                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di determinazione dello status commerciale       */
/*       *                                                       */
/*       * Solo se status diverso da 01                          */
/*       *-------------------------------------------------------*/
  sta_tud                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice cliente di riferimento in relazione allo       */
/*       * status del cliente, ovvero codice del nuovo cliente   */
/*       *                                                       */
/*       * Solo se status a 21,52,62,72                          */
/*       *-------------------------------------------------------*/
  sta_tuc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di trattamento nelle statistiche per i      */
/*       * documenti precedenti il cambiamento di status         */
/*       *                                                       */
/*       * Solo se codice cliente di riferimento diverso da zero */
/*       *                                                       */
/*       *  - 00 : Non significativo                             */
/*       *  - 01 : Statistiche distinte                          */
/*       *  - 02 : Statistiche girate sul cliente di riferimento */
/*       *         ma sempre sulla sua sede principale           */
/*       *  - 03 : Statistiche girate sul cliente di riferimento */
/*       *         conservando il codice dipendenza              */
/*       *=======================================================*/
  sta_tux                   tinyint (02)                not null,
/*       *=======================================================*/
/*       * Classe A..Z di importanza del cliente                 */
/*       *                                                       */
/*       * Questo campo puo' assumere i valori A..Z, oppure      */
/*       * spaces se valore non definito                         */
/*       *-------------------------------------------------------*/
  cld_imp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di rapporto commerciale                          */
/*       *                                                       */
/*       *  - 01 : Normale rapporto commerciale                  */
/*       *  - 10 : Rapporto di collaborazione                    */
/*       *  - 20 : Cliente che e' anche ns. concorrente          */
/*       *  - 41 : Cliente a noi associato                       */
/*       *  - 42 : Cliente cui siamo associati                   */
/*       *  - 51 : Cliente da noi controllato                    */
/*       *  - 52 : Cliente che ci controlla                      */
/*       *  - 61 : Cliente che e' nostro agente                  */
/*       *  - 62 : Cliente di cui siamo agenti                   */
/*       *  - 99 : Non definito                                  */
/*       *-------------------------------------------------------*/
  tra_pco                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Grado di introduzione commerciale                     */
/*       *                                                       */
/*       *  - 10 : Nessuna                                       */
/*       *  - 30 : Debole                                        */
/*       *  - 50 : Media                                         */
/*       *  - 70 : Forte                                         */
/*       *  - 99 : Non definita                                  */
/*       *-------------------------------------------------------*/
  gra_ico                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Pericolosita' della concorrenza                       */
/*       *                                                       */
/*       *  - 10 : Nessuna                                       */
/*       *  - 30 : Debole                                        */
/*       *  - 50 : Media                                         */
/*       *  - 70 : Forte                                         */
/*       *  - 99 : Non definita                                  */
/*       *-------------------------------------------------------*/
  pcl_ccz                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Previsione di continuita' nel rapporto commerciale    */
/*       *                                                       */
/*       *  - 01 : Cliente abituale                              */
/*       *  - 41 : Cliente saltuario                             */
/*       *  - 71 : Cliente occasionale                           */
/*       *  - 99 : Non definita                                  */
/*       *-------------------------------------------------------*/
  pre_ctn                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Origine del contatto con il cliente                   */
/*       *                                                       */
/*       *  - 01 : Da agente                                     */
/*       *  - 02 : Da personale di vendita interno               */
/*       *  - 03 : Da personale di vendita esterno               */
/*       *                                                       */
/*       *  - 21 : Segnalazione da cliente                       */
/*       *  - 22 : Segnalazione da collaboratore                 */
/*       *  - 23 : Segnalazione da produttore                    */
/*       *                                                       */
/*       *  - 41 : Pubblicita' generica                          */
/*       *  - 42 : Pubblicita' specifica                         */
/*       *  - 43 : Mailing                                       */
/*       *                                                       */
/*       *  - 61 : Presentazione spontanea casuale               */
/*       *  - 62 : Presentazione spontanea su ricerca            */
/*       *                                                       */
/*       *  - 99 : Non definita                                  */
/*       *-------------------------------------------------------*/
  org_ctt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice marketing                                      */
/*       *-------------------------------------------------------*/
  cod_mkt                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Indice libero marketing                               */
/*       *-------------------------------------------------------*/
  ind_mkt                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Classe di budget                                      */
/*       *=======================================================*/
  cla_bdg                   integer (05)                not null,
/*       *=======================================================*/
/*       * Segnale di cliente con accettazione ordini bloccata   */
/*       *                                                       */
/*       * - 01 : No, ordini accettabili                         */
/*       * - 02 : Si, ordini non accettabili                     */
/*       *-------------------------------------------------------*/
  for_blo                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco della       */
/*       * accettabilita' degli ordini                           */
/*       *-------------------------------------------------------*/
  dor_blo                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di cliente con consegne bloccate              */
/*       *                                                       */
/*       * - 01 : No, consegne effettuabili                      */
/*       * - 02 : Si, consegne non effettuabili                  */
/*       *-------------------------------------------------------*/
  fco_blo                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco delle       */
/*       * consegne                                              */
/*       *=======================================================*/
  dco_blo                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice condizioni di vendita da proporre come default */
/*       * in conferma ordine                                    */
/*       *-------------------------------------------------------*/
  cod_cdv                   integer (03)                not null,
/*       *=======================================================*/
/*       * Trasporto a cura abituale                             */
/*       *                                                       */
/*       *  - 01 : Nessuna preferenza                            */
/*       *                                                       */
/*       *  - 10 : Mittente                                      */
/*       *  - 11 : Mittente + Destinatario                       */
/*       *  - 12 : Mittente + Vettore                            */
/*       *  - 13 : Mittente + Vettore + Destinatario             */
/*       *  - 14 : Mittente + Vettore + Vettore                  */
/*       *                                                       */
/*       *  - 20 : Destinatario                                  */
/*       *                                                       */
/*       *  - 30 : Vettore                                       */
/*       *  - 31 : Vettore + Destinatario                        */
/*       *  - 32 : Vettore + Vettore                             */
/*       *  - 33 : Vettore + Vettore + Destinatario              */
/*       *  - 34 : Vettore + Vettore + Vettore                   */
/*       *-------------------------------------------------------*/
  dtt_acu                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice vettore abituale                               */
/*       *-------------------------------------------------------*/
  cod_vet                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * 2. vettore abituale                                   */
/*       *-------------------------------------------------------*/
  cod_vt2                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * 3. vettore abituale                                   */
/*       *-------------------------------------------------------*/
  cod_vt3                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero di abbonamento con il vettore                  */
/*       *-------------------------------------------------------*/
  abn_vtt                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Distanza in chilometri dalla ns. sede, o dalla ns.    */
/*       * dipendenza che normalmente serve il cliente           */
/*       *                                                       */
/*       * ___ LONGITUDINE E LATITUDINE                          */
/*       *-------------------------------------------------------*/
  dst_klm                   decimal (06,01)             not null,
/*       *-------------------------------------------------------*/
/*       * Settimana lavorativa, ovvero giorni di apertura       */
/*       *                                                       */
/*       * Ogni singolo carattere di questo campo indica un      */
/*       * giorno della settimana, il primo carattere sta' per   */
/*       * il Lunedi', ed il settimo per la Domenica             */
/*       *                                                       */
/*       * Ogni carattere puo' assumere il seguente valore:      */
/*       *                                                       */
/*       *  - A : Aperto tutto il giorno                         */
/*       *  - C : Chiuso tutto il giorno                         */
/*       *  - M : Aperto solamente al mattino                    */
/*       *  - P : Aperto solamente al pomeriggio                 */
/*       *                                                       */
/*       * Se questo campo e' tutto a spaces, esso sara' in-     */
/*       * terpretato come se fosse a 'AAAAACC', ovvero come     */
/*       * chiusura Sabato e Domenica                            */
/*       *-------------------------------------------------------*/
  gio_ac1                   varchar (01)                not null,
  gio_ac2                   varchar (01)                not null,
  gio_ac3                   varchar (01)                not null,
  gio_ac4                   varchar (01)                not null,
  gio_ac5                   varchar (01)                not null,
  gio_ac6                   varchar (01)                not null,
  gio_ac7                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Orario di apertura mattino, inizio                    */
/*       *-------------------------------------------------------*/
  oam_oin                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Orario di apertura mattino, fine                      */
/*       *-------------------------------------------------------*/
  oam_ofi                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Orario di apertura pomeriggio, inizio                 */
/*       *-------------------------------------------------------*/
  oap_oin                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Orario di apertura pomeriggio, fine                   */
/*       *=======================================================*/
  oap_ofi                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
