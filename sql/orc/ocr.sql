/*    *==========================================================*/
/*    * Tabella [ocr] Ordini clienti : righe               {orc} */
/*    *                                                          */
/*    * Aggiornamento 23.04.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        num_prg),
/* */
  KEY                       RCHMAG                     (cod_dpz,
                                                        flg_rch,
                                                        tip_mag,
                                                        num_mag,
                                                        num_prt,
                                                        num_prg),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null COMMENT 'Numero protocollo',
/*       *-------------------------------------------------------*/
/*       * Numero progressivo riga                               */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null COMMENT 'Numero progressivo riga',
/*       *-------------------------------------------------------*/
/*       * Tipo movimento                                        */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_tmo                   varchar (05)                not null COMMENT 'Tipo movimento',
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza che ha emesso il documento          */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null COMMENT 'Codice nostra dipendenza',
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null COMMENT 'Data documento',
/*       *-------------------------------------------------------*/
/*       * Numero documento : s.aa.xx.nnnnnn                     */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  num_doc                   bigint  (11)                not null COMMENT 'Numero documento',
/*       *-------------------------------------------------------*/
/*       * Tipo di ordine                                        */
/*       *                                                       */
/*       * - Spaces : normale                                    */
/*       * - P      : pro-forma (non impegna il magazzino)       */
/*       * - F      : forecast  (previsionale)                   */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  tip_ord                   varchar (01)                not null COMMENT 'Tipo di ordine',
/*       *-------------------------------------------------------*/
/*       * Tipo archivio per il movimento                        */
/*       *                                                       */
/*       * - C : Cliente                                         */
/*       * - D : Dipendenza                                      */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null COMMENT 'Tipo archivio',
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null COMMENT 'Codice archivio',
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza                                     */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null COMMENT 'Codice dipendenza archivio',
/*       *-------------------------------------------------------*/
/*       * Codice lingua per il documento                        */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null COMMENT 'Codice lingua documento',
/*       *-------------------------------------------------------*/
/*       * Data di riferimento ordine del cliente                */
/*       *-------------------------------------------------------*/
  ocl_dat                   date                        not null COMMENT 'Data di riferimento del cliente',
/*       *-------------------------------------------------------*/
/*       * Numero di riferimento ordine del cliente              */
/*       *-------------------------------------------------------*/
  ocl_num                   varchar (20)                not null COMMENT 'Numero di riferimento del cliente',
/*       *-------------------------------------------------------*/
/*       * Priorita' di evasione                                 */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  pri_eva                   varchar (02)                not null COMMENT 'Priorita di evasione',
/*       *-------------------------------------------------------*/
/*       * Valuta per la fatturazione                            */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  sgl_vpf                   varchar (03)                not null COMMENT 'Sigla valuta per la fatturazione',
  dec_vpf                   tinyint (01)                not null COMMENT 'Decimali valuta per la fatturazione',
  tdc_vpf                   varchar (01)                not null COMMENT 'Tipo cambio valuta per la fatturazione',
  cdc_vpf                   decimal (12,05)             not null COMMENT 'Cambio valuta per la fatturazione',
/*       *-------------------------------------------------------*/
/*       * Tipo riga                                             */
/*       *                                                       */
/*       * P   : Prodotti di vendita                             */
/*       * S   : Semilavorati                                    */
/*       * M   : Materie prime                                   */
/*       * PN  : Prodotti di vendita non codificati              */
/*       *                                                       */
/*       * A   : Addebito o Accredito a forfait                  */
/*       *                                                       */
/*       * C   : Commento                                        */
/*       *-------------------------------------------------------*/
  tip_rig                   varchar (05)                not null COMMENT 'Tipo riga',
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Prodotti di vendita    (Riga tipo 'P' o 'N')   */
/*       * - 02 : Semilavorati           (Riga tipo 'S')         */
/*       * - 03 : Materie prime          (Riga tipo 'M')         */
/*       * - 04 : Materiale vario        (Riga tipo 'V')         */
/*       *-------------------------------------------------------*/
  tip_mag                   tinyint (02)                not null COMMENT 'Tipo codice di magazzino',
/*       *-------------------------------------------------------*/
/*       * Codice numerico di magazzino                          */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga                           */
/*       *        - P  Codice numerico su [dcp]                  */
/*       *        - M  Codice numerico su [dpm]                  */
/*       *        - V  Codice numerico su [mtv]                  */
/*       *        - PN Codice numerico su [dcp]                  */
/*       *             del similare                              */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null COMMENT 'Codice numerico di magazzino',
/*       *-------------------------------------------------------*/
/*       * Codice alfanumerico di magazzino                      */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga                           */
/*       *        - P  Codice alfanumerico su [dcp]              */
/*       *        - M  Codice alfanumerico su [dpm]              */
/*       *        - V  Codice alfanumerico su [mtv]              */
/*       *        - PN Codice alfanumerico su [dcp]              */
/*       *             del similare                              */
/*       *-------------------------------------------------------*/
  alf_mag                   varchar (20)                not null COMMENT 'Codice alfanumerico di magazzino',
/*       *-------------------------------------------------------*/
/*       * Sigla della variante per il prodotto                  */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga                           */
/*       *        - P  Solo se codice [dcp] gestito a variante   */
/*       *        - S  Solo se codice [dps] gestito a variante   */
/*       *        - M  Solo se codice [dpm] gestito a variante   */
/*       *                                                       */
/*       *        - PN Solo se codice [dcp] del similare gestito */
/*       *             a variante                                */
/*       *                                                       */
/*       *        Questo valore non e' gestito direttamente dal  */
/*       *        programma di bollettazione; il campo puo' as-  */
/*       *        sumere un valore solo nel caso in cui il tipo  */
/*       *        origine del documento non sia manuale.         */
/*       *-------------------------------------------------------*/
  sgl_vrn                   varchar (20)                not null COMMENT 'Sigla variante',
/*       *-------------------------------------------------------*/
/*       * Codice prodotto per il cliente                        */
/*       *-------------------------------------------------------*/
  cop_scl                   varchar (20)                not null COMMENT 'Codice prodotto per il cliente',
/*       *-------------------------------------------------------*/
/*       * Presenza della descrizione estesa di max 400 caratte- */
/*       * ri in file di estensione                              */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       *                                                       */
/*       *       Cio' avviene nei seguenti casi                  */
/*       *       - Il tipo riga e' P,S,M, e la descrizione, pari */
/*       *         alla descrizione originale in anagrafica dcp] */
/*       *         o [dps] o [dpm], oppure variata rispetto ad   */
/*       *         essa, non supera i 40 caratteri               */
/*       *       - Il tipo riga e' PN,A,C, e la descrizione di-  */
/*       *         gitata non supera i 40 caratteri              */
/*       *                                                       */
/*       * - 1 : Si, in file di estensione descrizione per bol-  */
/*       *           lettazione [ocx]                            */
/*       *                                                       */
/*       *       Cio' avviene nei seguenti casi                  */
/*       *       - Il tipo riga e' P,S,M, e la descrizione, di-  */
/*       *         versa dalla descrizione originale in anagra-  */
/*       *         fica [dcp] o [dps] o [dpm], supera i 40 ca-   */
/*       *         ratteri                                       */
/*       *       - Il tipo riga e' PN,A,C, e la descrizione di-  */
/*       *         gitata supera i 40 caratteri                  */
/*       *                                                       */
/*       * - 2 : Si, in file di estensione descrizione per pro-  */
/*       *           dotti [pdx]                                 */
/*       *       - Il tipo riga e' P, e la descrizione, uguale   */
/*       *         alla descrizione originale in anagrafica      */
/*       *         [dcp], supera i 40 caratteri                  */
/*       *                                                       */
/*       * N.B. : Se il valore e' pari a 1, la descrizione este- */
/*       *        sa sostituisce completamente la descrizione di */
/*       *        40 caratteri contenuta in questo record.       */
/*       *-------------------------------------------------------*/
  des_ext                   tinyint (01)                not null COMMENT 'Presenza descrizione estesa',
/*       *-------------------------------------------------------*/
/*       * Descrizione per la riga di bolla                      */
/*       *                                                       */
/*       * - Se tipo riga P,S,M                                  */
/*       *                                                       */
/*       *   - Se des-ext 0 : Descrizione originale o modifica-  */
/*       *                    ta,  che comunque non supera glo-  */
/*       *                    balmente i 40 caratteri            */
/*       *                                                       */
/*       *   - Se des-ext 1 : Descrizione originale dell'anagra- */
/*       *                    fica [dcp] o [dps] o [dpm] di 40   */
/*       *                    caratteri                          */
/*       *                                                       */
/*       *   - Se des-ext 2 : Descrizione originale dell'anagra- */
/*       *                    fica [dcp] di 40 caratteri         */
/*       *                                                       */
/*       *                                                       */
/*       * - Se tipo riga PN,A,C                                 */
/*       *                                                       */
/*       *   - Se des-ext 0 : Descrizione impostata, la quale    */
/*       *                    non supera globalmente i 40 ca-    */
/*       *                    ratteri                            */
/*       *                                                       */
/*       *   - Se des-ext 1 : Primi 40 caratteri della descri-   */
/*       *                    zione impostata, la quale supera   */
/*       *                    globalmente i 40 caratteri         */
/*       *-------------------------------------------------------*/
  des_rig                   varchar (80)                not null COMMENT 'Descrizione per la riga',
/*       *-------------------------------------------------------*/
/*       * Tipo prodotto                                         */
/*       *                                                       */
/*       * - Se tipo riga P o PN                                 */
/*       *                                                       */
/*       *   - 01 : Merce                                        */
/*       *   - 02 : Servizio                                     */
/*       *   - 03 : Imballo                                      */
/*       *   - 09 : Extra attivita' aziendale                    */
/*       *                                                       */
/*       * - Se tipo riga S o M                                  */
/*       *                                                       */
/*       *   - 01 : Merce                                        */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  tip_pro                   tinyint (02)                not null COMMENT 'Tipo prodotto',
/*       *-------------------------------------------------------*/
/*       * Codice iva per la vendita                             */
/*       *-------------------------------------------------------*/
  cod_iva                   integer (05)                not null COMMENT 'Codice iva per la vendita',
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per la vendita                   */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (07)                not null COMMENT 'Codice contropartita per la vendita',
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per la vendita                       */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga                           */
/*       *        - P  Da anagrafica [dcp]                       */
/*       *        - S  Da anagrafica [dps]                       */
/*       *        - M  Da anagrafica [dpm]                       */
/*       *                                                       */
/*       *        - PN Da anagrafica [dcp] del similare          */
/*       *-------------------------------------------------------*/
  umi_ven                   varchar (03)                not null COMMENT 'Unita di misura per la vendita',
/*       *-------------------------------------------------------*/
/*       * Numero decimali quantita' per la vendita : 0..3       */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga                           */
/*       *        - P  Da anagrafica [dcp]                       */
/*       *        - S  Da anagrafica [dps]                       */
/*       *        - M  Da anagrafica [dpm]                       */
/*       *                                                       */
/*       *        - PN Da anagrafica [dcp] del similare          */
/*       *-------------------------------------------------------*/
  dec_qta                   tinyint (01)                not null COMMENT 'Decimali quantita - vendita',
/*       *-------------------------------------------------------*/
/*       * Quantita' ordinata                                    */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga P,M,V,PN                  */
/*       *-------------------------------------------------------*/
  qta_ord                   decimal (13,03)             not null COMMENT 'Quantita ordinata',
/*       *-------------------------------------------------------*/
/*       * Segnale di riga comunque considerata saldata          */
/*       * - Spaces : Non considerata saldata                    */
/*       * - #      : Considerata saldata                        */
/*       *                                                       */
/*       * Nota : Questo flag viene manipolato durante la fase   */
/*       *        di evasione ordini clienti, nella ratifica     */
/*       *        definitiva della fase di evasione, oppure ma-  */
/*       *        nualmente dal programma di gestione ordini     */
/*       *        clienti.                                       */
/*       *-------------------------------------------------------*/
  sdr_ccs                   varchar (01)                not null COMMENT 'Segnale di riga considerata saldata',
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione effettivo, da tipo movimento      */
/*       *-------------------------------------------------------*/
  cod_dsl                   varchar (07)                not null COMMENT 'Codice ubicazione',
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della seconda quantita' nella     */
/*       * fatturazione                                          */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *-------------------------------------------------------*/
  snx_2qt                   tinyint (01)                not null COMMENT 'S/N (0/1) II quantita',
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la seconda quantita'              */
/*       *                                                       */
/*       * N.B. : Significativi solo se 'si' gestione seconda    */
/*       *        quantita'                                      */
/*       *-------------------------------------------------------*/
  dec_2qt                   tinyint (01)                not null COMMENT 'Decimali II quantita',
/*       *-------------------------------------------------------*/
/*       * Seconda quantita'                                     */
/*       *                                                       */
/*       * N.B. : Significativi solo se 'si' gestione seconda    */
/*       *        quantita'                                      */
/*       *-------------------------------------------------------*/
  qta_a02                   decimal (13,03)             not null COMMENT 'II quantita',
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della terza quantita' nella       */
/*       * fatturazione                                          */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_3qt                   tinyint (01)                not null COMMENT 'S/N (0/1) III quantita',
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la terza quantita'                */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_3qt                   tinyint (01)                not null COMMENT 'Decimali III quantita',
/*       *-------------------------------------------------------*/
/*       * Terza quantita'                                       */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  qta_a03                   decimal (13,03)             not null COMMENT 'III quantita',
/*       *-------------------------------------------------------*/
/*       * Numero decimali per il prezzo di vendita              */
/*       *-------------------------------------------------------*/
  dec_prz                   tinyint (01)                not null COMMENT 'Decimali prezzo di vendita',
/*       *-------------------------------------------------------*/
/*       * Valuta per l'espressione dei prezzi standard          */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vps                   varchar (03)                not null COMMENT 'Valuta prezzi standard',
  dec_vps                   tinyint (01)                not null COMMENT 'Decimali valuta prezzi standard',
  tdc_vps                   varchar (01)                not null COMMENT 'Tipo cambio valuta prezzi standard',
  cdc_vps                   decimal (12,05)             not null COMMENT 'Cambio valuta prezzi standard',
/*       *-------------------------------------------------------*/
/*       * Prezzo lordo standard, in valuta di espressione dei   */
/*       * prezzi standard                                       */
/*       *-------------------------------------------------------*/
  prz_lrs                   bigint  (11)                not null COMMENT 'Prezzo lordo standard',
/*       *-------------------------------------------------------*/
/*       * Prezzo netto standard, in valuta di espressione dei   */
/*       * prezzi standard : pari al prezzo lordo standard e-    */
/*       * ventualmente abbattuto dagli sconti standard          */
/*       *-------------------------------------------------------*/
  prz_nts                   bigint  (11)                not null COMMENT 'Prezzo netto standard',
/*       *-------------------------------------------------------*/
/*       * Valuta per l'espressione del prezzo di vendita        */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vpp                   varchar (03)                not null COMMENT 'Sigla valuta prezzo di vendita',
  dec_vpp                   tinyint (01)                not null COMMENT 'Decimali valuta prezzo di vendita',
  tdc_vpp                   varchar (01)                not null COMMENT 'Tipo cambio valuta prezzo di vendita',
  cdc_vpp                   decimal (12,05)             not null COMMENT 'Cambio valuta prezzo di vendita',
/*       *-------------------------------------------------------*/
/*       * Flag di prezzo unitario relativo alla quantita'       */
/*       *                                                       */
/*       * - Spaces : prezzo unitario normale                    */
/*       * - K      : prezzo riferito a 1000 unita'              */
/*       * - C      : prezzo riferito a 100 unita'               */
/*       * - D      : prezzo riferito a 10 unita'                */
/*       *                                                       */
/*       * Nota : Questo flag influenza la gestione della secon- */
/*       *        da quantita' di fatturazione                   */
/*       *-------------------------------------------------------*/
  flg_puq                   varchar (01)                not null COMMENT 'Flag di prezzo unitario x quantita',
/*       *-------------------------------------------------------*/
/*       * Prezzo unitario di vendita, in valuta                 */
/*       *-------------------------------------------------------*/
  prz_ven                   bigint  (11)                not null COMMENT 'Prezzo unitario di vendita, in valuta',
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione del secondo prezzo nella fattura- */
/*       * zione                                                 */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_2pz                   tinyint (01)                not null COMMENT 'S/N (0/1) II prezzo',
/*       *-------------------------------------------------------*/
/*       * Numero decimali del secondo prezzo                    */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_2pz                   tinyint (01)                not null COMMENT 'Decimali II prezzo',
/*       *-------------------------------------------------------*/
/*       * Secondo prezzo, in valuta                             */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  prz_a02                   bigint  (11)                not null COMMENT 'II prezzo',
/*       *-------------------------------------------------------*/
/*       * Valuta per l'espressione del legame valutario         */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Prezzo di riferimento, in valuta                    */
/*       * - Cambio                                              */
/*       * - Cambio di riferimento                               */
/*       * - Percentuale di limitazione                          */
/*       * - Tipo di limitazione                                 */
/*       *   - T : Tolleranza                                    */
/*       *   - F : Franchigia                                    */
/*       *   - M : Massimo tetto                                 */
/*       * - Momento di applicazione                             */
/*       *   - F : Al momento della fattura                      */
/*       *   - B : Al momento della bolla                        */
/*       *-------------------------------------------------------*/
  sgl_vpl                   varchar (03)                not null COMMENT 'Valuta legame valutario',
  dec_vpl                   tinyint (01)                not null COMMENT 'Decimali valuta legame valutario',
  tdc_vpl                   varchar (01)                not null COMMENT 'Tipo cambio valuta legame valutario',
  prz_vpl                   bigint  (11)                not null COMMENT 'Prezzo di riferimento legame valutario',
  cdc_vpl                   decimal (12,05)             not null COMMENT 'Cambio legame valutario',
  ccr_vpl                   decimal (12,05)             not null COMMENT 'Cambio di riferimento legame valutario',
  plm_vpl                   decimal (05,02)             not null COMMENT 'Percentuale di limitazione legame valutario',
  tlm_vpl                   varchar (01)                not null COMMENT 'Tipo di limitazione legame valutario',
  map_vpl                   varchar (01)                not null COMMENT 'Momento di applicazione legame valutario',
/*       *-------------------------------------------------------*/
/*       * Tipo di esposizione della riga di fattura             */
/*       *                                                       */
/*       * - Se tipo riga P o Pn                                 */
/*       *                                                       */
/*       *    - 1 : Quantita', prezzo, sconti, importo           */
/*       *    - 2 : Solo quantita' e importo                     */
/*       *    - 3 : Solo importo                                 */
/*       *    - 4 : Solo importo se quantita pari a 1,000, al-   */
/*       *               trimenti tutto come il tipo 1           */
/*       *    - 5 : Solo importo se quantita pari a 1,000, al-   */
/*       *               trimenti tutto come il tipo 2           */
/*       *                                                       */
/*       * - Se tipo riga S o M                                  */
/*       *                                                       */
/*       *    - 1 : Quantita', prezzo, sconti, importo           */
/*       *-------------------------------------------------------*/
  epz_rgf                   tinyint (01)                not null COMMENT 'Tipo esposizione riga fattura',
/*       *-------------------------------------------------------*/
/*       * Categoria di sconto in riga associata al prodotto     */
/*       *-------------------------------------------------------*/
  csr_aap                   integer (05)                not null COMMENT 'Categoria sconto associata al prodotto',
/*       *-------------------------------------------------------*/
/*       * Precentuali di sconto in riga associate al prodotto   */
/*       *-------------------------------------------------------*/
  psr_ap1                   decimal (05,02)             not null COMMENT 'Percentuale sconto associata al prodotto',
  psr_ap2                   decimal (05,02)             not null COMMENT 'Percentuale sconto associata al prodotto',
  psr_ap3                   decimal (05,02)             not null COMMENT 'Percentuale sconto associata al prodotto',
  psr_ap4                   decimal (05,02)             not null COMMENT 'Percentuale sconto associata al prodotto',
  psr_ap5                   decimal (05,02)             not null COMMENT 'Percentuale sconto associata al prodotto',
/*       *-------------------------------------------------------*/
/*       * Percentuali di sconto in riga 1..5                    */
/*       *-------------------------------------------------------*/
  per_sr1                   decimal (05,02)             not null COMMENT 'Percentuale di sconto in riga',
  per_sr2                   decimal (05,02)             not null COMMENT 'Percentuale di sconto in riga',
  per_sr3                   decimal (05,02)             not null COMMENT 'Percentuale di sconto in riga',
  per_sr4                   decimal (05,02)             not null COMMENT 'Percentuale di sconto in riga',
  per_sr5                   decimal (05,02)             not null COMMENT 'Percentuale di sconto in riga',
/*       *-------------------------------------------------------*/
/*       * Prezzo netto effettivo, in valuta                     */
/*       *-------------------------------------------------------*/
  prz_net                   bigint  (11)                not null COMMENT 'Prezzo netto effettivo, in valuta',
/*       *-------------------------------------------------------*/
/*       * Tipo di esposizione prezzi e sconti della riga di     */
/*       * fattura                                               */
/*       *                                                       */
/*       * - Se tipo riga P o PN                                 */
/*       *                                                       */
/*       *    - 11 : Prezzo di vendita e sconti                  */
/*       *    - 21 : Solo prezzo netto                           */
/*       *-------------------------------------------------------*/
  epz_pes                   tinyint (02)                not null COMMENT 'Tipo esposizione prezzi e sconti in riga',
/*       *-------------------------------------------------------*/
/*       * Valuta per l'espressione del costo di riferimento     */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vpc                   varchar (03)                not null COMMENT 'Sigla valuta costo di riferimento',
  dec_vpc                   tinyint (01)                not null COMMENT 'Decimali valuta costo di riferimento',
  tdc_vpc                   varchar (01)                not null COMMENT 'Tipo cambio valuta costo di riferimento',
  cdc_vpc                   decimal (12,05)             not null COMMENT 'Cambio valuta costo di riferimento',
/*       *-------------------------------------------------------*/
/*       * Numero decimali per il costo di riferimento : 0..2    */
/*       *-------------------------------------------------------*/
  dec_cos                   tinyint (01)                not null COMMENT 'Numero decimali per il costo di riferimento',
/*       *-------------------------------------------------------*/
/*       * Costo di riferimento, in valuta                       */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  cos_rif                   bigint  (11)                not null COMMENT 'Costo di riferimento',
/*       *-------------------------------------------------------*/
/*       * Importo definitivo per la riga, in valuta             */
/*       *-------------------------------------------------------*/
  imp_rig                   bigint  (13)                not null COMMENT 'Importo riga. in valuta',
/*       *-------------------------------------------------------*/
/*       * Importo ausiliario per gestione blocchi, in valuta    */
/*       *                                                       */
/*       * ___ ELIMINARE ??? ___                                 */
/*       *-------------------------------------------------------*/
  iau_rig                   bigint  (13)                not null COMMENT 'Importo ausiliario per gestione blocchi',
/*       *-------------------------------------------------------*/
/*       * Categoria di provvigione associata al prodotto        */
/*       *-------------------------------------------------------*/
  cpv_aap                   integer (05)                not null COMMENT 'Categoria di provvigione associata al prodotto',
/*       *-------------------------------------------------------*/
/*       * % di provvigioni associate al prodotto                */
/*       *-------------------------------------------------------*/
  ppv_ap1                   decimal (05,02)             not null COMMENT 'Percentuale provvigione prodotto',
  ppv_ap2                   decimal (05,02)             not null COMMENT 'Percentuale provvigione prodotto',
  ppv_ap3                   decimal (05,02)             not null COMMENT 'Percentuale provvigione prodotto',
/*       *-------------------------------------------------------*/
/*       * Flag di significativita' dati provvigionali in riga   */
/*       *                                                       */
/*       * - 01 : Si, calcolate automaticamente                  */
/*       * - 02 : No provvigioni in riga                         */
/*       * - 03 : Si, speciali                                   */
/*       *-------------------------------------------------------*/
  fsp_rig                   tinyint (02)                not null COMMENT 'Trattamento provvigioni in riga',
/*       *-------------------------------------------------------*/
/*       * Categoria di provvigione in riga effettiva            */
/*       *-------------------------------------------------------*/
  cpv_rig                   integer (05)                not null COMMENT 'Categoria provvigione in riga effettiva',
/*       *-------------------------------------------------------*/
/*       * % di provvigioni in riga effettive                    */
/*       *-------------------------------------------------------*/
  ppv_ri1                   decimal (05,02)             not null COMMENT 'Percentuale provvigione in riga effettiva',
  ppv_ri2                   decimal (05,02)             not null COMMENT 'Percentuale provvigione in riga effettiva',
  ppv_ri3                   decimal (05,02)             not null COMMENT 'Percentuale provvigione in riga effettiva',
/*       *-------------------------------------------------------*/
/*       * Importo provvigione a forfait in riga, in valuta base */
/*       *-------------------------------------------------------*/
  pvf_rig                   bigint  (13)                not null COMMENT 'Importo provvigione a forfait in riga',
/*       *-------------------------------------------------------*/
/*       * Data di consegna richiesta                            */
/*       *-------------------------------------------------------*/
  dcn_ric                   date                        not null COMMENT 'Data consegna richiesta',
/*       *-------------------------------------------------------*/
/*       * Data di consegna prevista                             */
/*       *-------------------------------------------------------*/
  dcn_prv                   date                        not null COMMENT 'Data consegna prevista',
/*       *-------------------------------------------------------*/
/*       * Data di consegna confermata                           */
/*       *-------------------------------------------------------*/
  dcn_cnf                   date                        not null COMMENT 'Data consegna confermata',
/*       *-------------------------------------------------------*/
/*       * Flag di conferma a cliente                            */
/*       *                                                       */
/*       * - 0 : Riga ancora da confermare                       */
/*       * - 1 : Riga confermata a cliente                       */
/*       *-------------------------------------------------------*/
  flg_cnf                   tinyint (01)                not null COMMENT 'Flag di conferma a cliente',
/*       *-------------------------------------------------------*/
/*       * Tipo documento nostra commissione al cliente          */
/*       *-------------------------------------------------------*/
  cmc_tip                   varchar (05)                not null COMMENT 'Tipo documento nostra commissione al cliente',
/*       *-------------------------------------------------------*/
/*       * Data di riferimento nostra commissione al cliente     */
/*       *-------------------------------------------------------*/
  cmc_dat                   date                        not null COMMENT 'Data di riferimento nostra commissione al cliente',
/*       *-------------------------------------------------------*/
/*       * Numero di riferimento nostra commissione al cliente   */
/*       *-------------------------------------------------------*/
  cmc_num                   bigint  (11)                not null COMMENT 'Numero di riferimento nostra commissione al cliente',
/*       *-------------------------------------------------------*/
/*       * Flag di riga completamente chiusa                     */
/*       *                                                       */
/*       * - 0 : Non ancora completamente chiusa                 */
/*       * - 1 : Completamente chiusa                            */
/*       *-------------------------------------------------------*/
  flg_rch                   tinyint (01)                not null COMMENT 'Flag di riga completamente chiusa',
/*       *-------------------------------------------------------*/
/*       * Flags di elaborazione bloccanti                       */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_bl1                   tinyint (01)                not null COMMENT 'Flag di elaborazione bloccante',
  flg_bl2                   tinyint (01)                not null COMMENT 'Flag di elaborazione bloccante',
  flg_bl3                   tinyint (01)                not null COMMENT 'Flag di elaborazione bloccante',
  flg_bl4                   tinyint (01)                not null COMMENT 'Flag di elaborazione bloccante',
  flg_bl5                   tinyint (01)                not null COMMENT 'Flag di elaborazione bloccante',
/*       *-------------------------------------------------------*/
/*       * Flags di elaborazione NON bloccanti                   */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   tinyint (01)                not null COMMENT 'Flag di elaborazione non bloccante',
  flg_nb2                   tinyint (01)                not null COMMENT 'Flag di elaborazione non bloccante',
  flg_nb3                   tinyint (01)                not null COMMENT 'Flag di elaborazione non bloccante',
  flg_nb4                   tinyint (01)                not null COMMENT 'Flag di elaborazione non bloccante',
  flg_nb5                   tinyint (01)                not null COMMENT 'Flag di elaborazione non bloccante',
/*       *-------------------------------------------------------*/
/*       * Flag di riga verificata                               */
/*       *                                                       */
/*       * - 0 : Verificata                                      */
/*       * - 1 : Da verificare                                   */
/*       *-------------------------------------------------------*/
  flg_ver                   tinyint (01)                not null COMMENT 'Flag di riga verificata',
/*       *-------------------------------------------------------*/
/*       * Flag di riga ordine con data prevista forzata         */
/*       *                                                       */
/*       * - 0 : nessuna forzatura                               */
/*       * - 1 : data prevista forzata                           */
/*       *-------------------------------------------------------*/
  flg_dpf                   tinyint (01)                not null COMMENT 'Flag di data prevista forzata',
/*       *-------------------------------------------------------*/
/*       * Flag di controllo in fase di spedizione               */
/*       *                                                       */
/*       * - 0 : riga non ancora controllata                     */
/*       * - 1 : riga controllata                                */
/*       *-------------------------------------------------------*/
  flg_spn                   tinyint (01)                not null COMMENT 'Flag di controllo in fase di spedizione'
