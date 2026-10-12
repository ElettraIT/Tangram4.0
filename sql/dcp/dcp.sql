/*    *==========================================================*/
/*    * Tabella [dcp] Prodotti                             {dcp} */
/*    *                                                          */
/*    * Aggiornamento 04.05.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_pro),
/*       *-------------------------------------------------------*/
/*       * Chiavi addizionali                                    */
/*       *-------------------------------------------------------*/
  KEY                       ALFPRO                     (alf_pro,
                                                        num_pro),
/* */
  KEY                       CGSALF                     (cla_pro,
                                                        gru_pro,
                                                        sgr_pro,
                                                        alf_pro,
                                                        num_pro),
/*       *=======================================================*/
/*       * Codice numerico prodotto                              */
/*       *-------------------------------------------------------*/
  num_pro                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto alfanumerico                          */
/*       *-------------------------------------------------------*/
  alf_pro                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Sinonimo prodotto                                     */
/*       *-------------------------------------------------------*/
  syn_pro                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Chiave alfanumerica libera per il prodotto            */
/*       *-------------------------------------------------------*/
  klb_pro                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione prodotto in lingua locale                 */
/*       *-------------------------------------------------------*/
  des_pro                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione prodotto in lingua per i documenti        */
/*       *-------------------------------------------------------*/
  des_doc                   text                        not null,
/*       *-------------------------------------------------------*/
/*       * Classificazione merceologica del prodotto             */
/*       *-------------------------------------------------------*/
  cla_pro                   integer (05)                not null,
  gru_pro                   integer (05)                not null,
  sgr_pro                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo prodotto                                         */
/*       *                                                       */
/*       *  - 01 : Merce                                         */
/*       *  - 02 : Servizio                                      */
/*       *  - 03 : Imballo                                       */
/*       *  - 09 : Extra attivita' aziendale                     */
/*       *-------------------------------------------------------*/
  tip_pro                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo acquisizione prodotto                            */
/*       *                                                       */
/*       *  - 01 : Fornito da terzi                              */
/*       *  - 11 : Di nostra produzione                          */
/*       *  - 21 : Indifferentemente fornito da terzi o di       */
/*       *         nostra produzione                             */
/*       *-------------------------------------------------------*/
  tip_acp                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Nota generica sul prodotto                            */
/*       *-------------------------------------------------------*/
  not_pro                   text                        not null,
/*       *-------------------------------------------------------*/
/*       * Si/No prodotto in confezione                          */
/*       *                                                       */
/*       *  - 0 : No, il prodotto non e' una confezione          */
/*       *  - 1 : Si, il prodotto e' una confezione              */
/*       *-------------------------------------------------------*/
  tip_cfz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' per confezione                              */
/*       *                                                       */
/*       * Solo se Si/No prodotto in confezione a 01             */
/*       *-------------------------------------------------------*/
  qta_cfz                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peso unitario del prodotto, lordo                     */
/*       *-------------------------------------------------------*/
  pes_uni                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peso tara per il prodotto                             */
/*       *-------------------------------------------------------*/
  pes_tar                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Volume unitario del prodotto                          */
/*       *-------------------------------------------------------*/
  vol_uni                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Dimensioni del prodotto                               */
/*       *                                                       */
/*       * - Larghezza                                           */
/*       * - Altezza                                             */
/*       * - Profondita'                                         */
/*       *-------------------------------------------------------*/
  dim_lar                   decimal (10,03)             not null,
  dim_alt                   decimal (10,03)             not null,
  dim_prf                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Peculiarita' fisiche specifiche del prodotto          */
/*       *-------------------------------------------------------*/
  pcl_fis                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Coefficiente moltiplicatore libero                    */
/*       *-------------------------------------------------------*/
  coe_mol                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Coefficiente divisore libero                          */
/*       *-------------------------------------------------------*/
  coe_div                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Specifica libera per il prodotto                      */
/*       *-------------------------------------------------------*/
  spc_lib                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice Iva per la vendita                             */
/*       *-------------------------------------------------------*/
  cod_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Conto di contabilita' generale usato come contropar-  */
/*       * tita per le vendite del prodotto                      */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per la vendita                       */
/*       *-------------------------------------------------------*/
  umi_ven                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali quantita' per la vendita : 0..3       */
/*       *-------------------------------------------------------*/
  dec_qta                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla valuta                                          */
/*       *-------------------------------------------------------*/
  sgl_vlt                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la valuta                         */
/*       *-------------------------------------------------------*/
  dec_vlt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per il prezzo di vendita : 0..2       */
/*       *-------------------------------------------------------*/
  dec_prz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo di listino di base, in valuta base             */
/*       *-------------------------------------------------------*/
  prz_lst                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Lotto di vendita                                      */
/*       *-------------------------------------------------------*/
  lot_ven                   decimal (10,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di esposizione della riga di fattura             */
/*       * - 1 : Quantita', prezzo, sconti, importo              */
/*       * - 2 : Solo quantita' e importo                        */
/*       * - 3 : Solo importo                                    */
/*       * - 4 : Solo importo se quantita pari a 1,000, al-      */
/*       *       trimenti tutto come il tipo 1                   */
/*       * - 5 : Solo importo se quantita pari a 1,000, al-      */
/*       *       trimenti tutto come il tipo 2                   */
/*       *-------------------------------------------------------*/
  epz_rgf                   tinyint (01)                not null,
  
  
/*       *-------------------------------------------------------*/
/*       * ___ ELIMINARE ??? ___                                 */
/*       *-------------------------------------------------------*/
  
  
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della seconda quantita' nella     */
/*       * fatturazione                                          */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_qt2                   tinyint (01)      not null default 0,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la seconda quantita'              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_qt2                   tinyint (01)      not null default 0,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della terza quantita' nella       */
/*       * fatturazione                                          */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_qt3                   tinyint (01)      not null default 0,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la terza quantita'                */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_qt3                   tinyint (01)      not null default 0,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione del secondo prezzo nella fattura- */
/*       * zione                                                 */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_pz2                   tinyint (01)      not null default 0,
/*       *-------------------------------------------------------*/
/*       * Decimali del secondo prezzo nella fatturazione        */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_pz2                   tinyint (01)      not null default 0,
  
  
/*       *-------------------------------------------------------*/
/*       * ___ ELIMINARE ??? ___                                 */
/*       *-------------------------------------------------------*/
  
  
/*       *-------------------------------------------------------*/
/*       * Codice categoria sconto in riga                       */
/*       *-------------------------------------------------------*/
  cat_scr                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % di sconto in riga 1..5                              */
/*       *-------------------------------------------------------*/
  per_sr1                   decimal (05,02)             not null,
  per_sr2                   decimal (05,02)             not null,
  per_sr3                   decimal (05,02)             not null,
  per_sr4                   decimal (05,02)             not null,
  per_sr5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria provvigionale per il prodotto        */
/*       *-------------------------------------------------------*/
  cat_pvg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % provvigioni per il prodotto 1..3                    */
/*       *                                                       */
/*       * N.B. : Le percentuali 2. e 3. non sono gestite        */
/*       *-------------------------------------------------------*/
  per_pv1                   decimal (05,02)             not null,
  per_pv2                   decimal (05,02)             not null,
  per_pv3                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare provvigioni per la quantita' unitaria di    */
/*       * prodotto, in valuta pari a quella del prezzo di       */
/*       * listino                                               */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  amm_pvg                   integer (09)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di prodotto gestito nei listini di vendita    */
/*       *                                                       */
/*       * - 0 : Si                                              */
/*       * - 1 : No                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_lst                   tinyint (01)      not null default 0,
/*       *-------------------------------------------------------*/
/*       * Codici statistici prodotto                            */
/*       *-------------------------------------------------------*/
  cod_s01                   integer (05)                not null,
  cod_s02                   integer (05)                not null,
  cod_s03                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di inizio commercializzazione del prodotto       */
/*       *-------------------------------------------------------*/
  dat_icm                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Status commerciale del prodotto                       */
/*       *                                                       */
/*       *  - 01 : Normale                                       */
/*       *  - 11 : Ad esaurimento                                */
/*       *  - 21 : Sostituito da ns. nuovo prodotto              */
/*       *  - 51 : Cessata commercializzazione                   */
/*       *  - 52 : Cessata commercializzazione, ma sostituito da */
/*       *         ns. nuovo prodotto                            */
/*       *  - 71 : Obsoleto                                      */
/*       *  - 72 : Obsoleto, ma sostituito da ns. nuovo prodotto */
/*       *-------------------------------------------------------*/
  sta_tus                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di determinazione dello status commerciale       */
/*       *                                                       */
/*       * Solo se status diverso da 01                          */
/*       *-------------------------------------------------------*/
  sta_tud                   date                            null,
/*       *-------------------------------------------------------*/
/*       * Codice numerico prodotto di riferimento in relazione  */
/*       * allo status del prodotto, ovvero codice numerico del  */
/*       * nuovo prodotto                                        */
/*       *                                                       */
/*       * Solo se status a 21,52,72                             */
/*       *-------------------------------------------------------*/
  sta_tuc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di trattamento nelle statistiche per i      */
/*       * documenti precedenti il cambiamento di status         */
/*       *                                                       */
/*       * Solo se codice prodotto di riferim. diverso da zero   */
/*       *                                                       */
/*       *  - 00 : Non significativo                             */
/*       *  - 01 : Statistiche distinte                          */
/*       *  - 02 : Statistiche girate su prodotto di riferimento */
/*       *-------------------------------------------------------*/
  sta_tux                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Classe A..Z di importanza del prodotto                */
/*       *                                                       */
/*       * Questo campo puo' assumere i valori A..Z, oppure      */
/*       * spaces se valore non definito                         */
/*       *-------------------------------------------------------*/
  cld_imp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Previsione di continuita' della commercializzazione   */
/*       * del prodotto                                          */
/*       *                                                       */
/*       *  - 01 : Prodotto di abituale commercializzazione      */
/*       *  - 41 : Prodotto di saltuaria commercializzazione     */
/*       *  - 71 : Prodotto di occasionale commercializzazione   */
/*       *  - 99 : Non definita                                  */
/*       *-------------------------------------------------------*/
  pre_ctn                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Grado di introduzione commerciale del prodotto        */
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
/*       * Codice marketing per il prodotto                      */
/*       *-------------------------------------------------------*/
  cod_mkt                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Indice libero marketing per il prodotto               */
/*       *-------------------------------------------------------*/
  ind_mkt                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice classe di budget di vendita per il prodotto    */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  cla_bdg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di prodotto con accettazione ordini bloccata  */
/*       *                                                       */
/*       * - 0 : No, ordini accettabili                          */
/*       * - 1 : Si, ordini non accettabili                      */
/*       *-------------------------------------------------------*/
  for_blo                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco della       */
/*       * accettabilita' degli ordini                           */
/*       *-------------------------------------------------------*/
  dor_blo                   date                            null,
/*       *-------------------------------------------------------*/
/*       * Segnale di prodotto con consegna bloccata             */
/*       *                                                       */
/*       * - 0 : No, consegne effettuabili                       */
/*       * - 1 : Si, consegne non effettuabili                   */
/*       *-------------------------------------------------------*/
  fco_blo                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Data in cui e' avvenuto il blocco/sblocco delle       */
/*       * consegne                                              */
/*       *-------------------------------------------------------*/
  dco_blo                   date                            null,
/*       *-------------------------------------------------------*/
/*       * Codice della nomenclatura combinata della merce per   */
/*       * la gestione IVA Intracomunitaria                      */
/*       *                                                       */
/*       * PENSARE A CODICI 'ISO'                                */
/*       *-------------------------------------------------------*/
  cdn_cdm                   integer (08)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice addebito legato al prodotto                    */
/*       *                                                       */
/*       * N.B.: Gestita solo in presenza di apposita personal.  */
/*       *                                                       */
/*       * Serviva per collegare ai listini RAEE                 */
/*       *-------------------------------------------------------*/
  add_pro                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Mese inizio stagione                                  */
/*       *-------------------------------------------------------*/
  mes_sti                   tinyint (02)                    null,
/*       *-------------------------------------------------------*/
/*       * Mese fine stagione                                    */
/*       *-------------------------------------------------------*/
  mes_stf                   tinyint (02)                    null,
/*       *-------------------------------------------------------*/
/*       * Percorso immagine                                     */
/*       *-------------------------------------------------------*/
  pth_img                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Percorso scheda tecnica o altro PDF                   */
/*       *-------------------------------------------------------*/
  pth_pdf                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
