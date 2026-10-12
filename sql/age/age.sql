/*    *==========================================================*/
/*    * Tabella [age] Agenti                                     */
/*    *                                                          */
/*    * Aggiornamento 19.12.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_age),
/*       *=======================================================*/
/*       * Codice agente                                         */
/*       *                                                       */
/*       * Nota: Se un agente e' anche contemporaneamente un     */
/*       *       super-agente e' consigliabile codificare due    */
/*       *       codici agente separati, uno come agente e u-    */
/*       *       no come super-agente.                           */
/*       *       La codifica consigliata e' la seguente:         */
/*       *       - Codici 0000001..0009999 : agenti              */
/*       *       - Codici 0010001..0019999 : super-agenti        */
/*       *-------------------------------------------------------*/
  cod_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Mnemonico                                             */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Nominativo                                            */
/*       *-------------------------------------------------------*/
  nom_age                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Ragione sociale                                       */
/*       *-------------------------------------------------------*/
  rag_soc                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Indirizzo                                             */
/*       *-------------------------------------------------------*/
  via_age                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Localita                                              */
/*       *-------------------------------------------------------*/
  loc_age                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nazione                                        */
/*       *-------------------------------------------------------*/
  cod_naz                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice comune                                         */
/*       *-------------------------------------------------------*/
  cod_cmn                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice frazione                                       */
/*       *-------------------------------------------------------*/
  cod_fzn                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice localita                                       */
/*       *-------------------------------------------------------*/
  cod_lct                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Partita IVA                                           */
/*       *-------------------------------------------------------*/
  prt_iva                   varchar (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice fiscale                                        */
/*       *-------------------------------------------------------*/
  cod_fis                   varchar (16)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice sottoconto                                     */
/*       *-------------------------------------------------------*/
  cod_cge                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente cui questo agente fa capo               */
/*       *                                                       */
/*       * N.B. : Questo campo viene manutenuto dal programma di */
/*       *        manutenzione dei legami per data tra agenti e  */
/*       *        super-agenti, che provvedera' ad aggiornarlo   */
/*       *        con l'ultimo valore valido.                    */
/*       *        In ogni caso per sapere, con riferimento ad u- */
/*       *        na certa data, quale sia l'eventuale super-    */
/*       *        agente di un agente, sara' necessario esegui-  */
/*       *        re una scansione sul file [ags].               */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  sup_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice categoria provvigionale agente                 */
/*       *-------------------------------------------------------*/
  cat_pvg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % di provvigione per agente                           */
/*       *                                                       */
/*       * Nota : Le percentuali 2. e 3. sono forzate comunque a */
/*       *        zero                                           */
/*       *-------------------------------------------------------*/
  per_pv1                   decimal (05,02)             not null,
  per_pv2                   decimal (05,02)             not null,
  per_pv3                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice classe di budget vendita agente                */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  cla_bdg                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di maturazione                                   */
/*       *                                                       */
/*       *  - spaces : non significativo                         */
/*       *                                                       */
/*       *  - I      : Maturazione immediata dei conteggi        */
/*       *             provvigionali                             */
/*       *                                                       */
/*       *  - P      : Maturazione dei conteggi provvigionali    */
/*       *             a fronte pagamenti                        */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  tip_mat                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags liberi per elaborazione conteggi provvigionali  */
/*       *-------------------------------------------------------*/
  flg_p01                   varchar (01)                not null,
  flg_p02                   varchar (01)                not null,
  flg_p03                   varchar (01)                not null,
  flg_p04                   varchar (01)                not null,
  flg_p05                   varchar (01)                not null,
  flg_p06                   varchar (01)                not null,
  flg_p07                   varchar (01)                not null,
  flg_p08                   varchar (01)                not null,
  flg_p09                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Contropartita per le vendite effettuate dall'agente   */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Contropartita per i resi su vendite effettuate dal-   */
/*       * l'agente                                              */
/*       *-------------------------------------------------------*/
  ctp_rsv                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Targhe automezzo                                      */
/*       *-------------------------------------------------------*/
  trg_au1                   varchar (20)                not null,
  trg_au2                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di inizio attivita'                              */
/*       *-------------------------------------------------------*/
  dat_iat                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Status commerciale agente                             */
/*       *                                                       */
/*       *  - 01 : Normale                                       */
/*       *  - 11 : Esauriti i rapporti commerciali               */
/*       *  - 21 : Sostituito da ns. nuovo agente                */
/*       *  - 51 : Cessata attivita'                             */
/*       *  - 52 : Cessata attivita', ma sostituito da ns.       */
/*       *         nuovo agente                                  */
/*       *  - 61 : In contenzioso                                */
/*       *  - 62 : In contenzioso, ma sostituito da ns. nuo-     */
/*       *         vo agente                                     */
/*       *  - 71 : Fallito                                       */
/*       *  - 72 : Fallito, ma sostituito da ns. nuovo agen-     */
/*       *         te                                            */
/*       *-------------------------------------------------------*/
  sta_tus                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di determinazione dello status                   */
/*       *                                                       */
/*       * Solo se status diverso da 01                          */
/*       *-------------------------------------------------------*/
  sta_tud                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente di riferimento in relazione allo        */
/*       * status dell' agente, ovvero codice del nuovo agente   */
/*       *                                                       */
/*       * Solo se status a 21,52,62,72                          */
/*       *-------------------------------------------------------*/
  sta_tuc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di trattamento nelle statistiche per i      */
/*       * documenti precedenti il cambiamento di status         */
/*       *                                                       */
/*       * Solo se codice agente di riferimento diverso da       */
/*       * zero                                                  */
/*       *                                                       */
/*       *  - 00 : Non significativo                             */
/*       *  - 01 : Statistiche distinte                          */
/*       *  - 02 : Statistiche girate sull' agente di rife-      */
/*       *         rimento                                       */
/*       *-------------------------------------------------------*/
  sta_tux                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza di pertinenza 1                     */
/*       *-------------------------------------------------------*/
  dpz_ap1                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza di pertinenza 2                     */
/*       *-------------------------------------------------------*/
  dpz_ap2                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
