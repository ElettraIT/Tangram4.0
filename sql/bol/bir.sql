/*    *==========================================================*/
/*    * Tabella [bir] Bolle clienti : righe                {bol} */
/*    *                                                          */
/*    * Aggiornamento 17.09.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
/*       *                                                       */
/*       * Indici : "NUMPRT    " = num_prt                       */
/*       *                         num_prg                       */
/*       *                                                       */
/*       *          "MAGDAT    " = cod_dpz                       */
/*       *                         tip_mag                       */
/*       *                         num_mag                       */
/*       *                         dat_doc                       */
/*       *                         num_prt                       */
/*       *                         num_prg                       */
/*       *                                                       */
/*       *          "RIFORC    " = cod_dpz                       */
/*       *                         coc_prt                       */
/*       *                         coc_prg                       */
/*       *                         num_prt                       */
/*       *                         num_prg                       */
/*       *                                                       */
/*       *          "RIFODS    " = cod_dpz                       */
/*       *                         ods_prt                       */
/*       *                         ods_prg                       */
/*       *                         coc_prt                       */
/*       *                         coc_prg                       */
/*       *                         num_prt                       */
/*       *                         num_prg                       */
/*       *                                                       */
/*       *          "ARCRIG    " = cod_dpz   ___ DA IMPLEM ___   */
/*       *                         flg_rch                       */
/*       *                         tip_arc                       */
/*       *                         cod_arc                       */
/*       *                         num_prt                       */
/*       *                         num_prg                       */
/*       *                                                       */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (num_prt,
                                                         num_prg),
/* */
  KEY                       MAGDAT                      (cod_dpz,
                                                         tip_mag,
                                                         num_mag,
                                                         dat_doc,
                                                         num_prt,
                                                         num_prg),
/* */
  KEY                       RIFORC                      (cod_dpz,
                                                         coc_prt,
                                                         coc_prg,
                                                         num_prt,
                                                         num_prg),
/* */
  KEY                       RIFODS                      (cod_dpz,
                                                         ods_prt,
                                                         ods_prg,
                                                         coc_prt,
                                                         coc_prg,
                                                         num_prt,
                                                         num_prg),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo riga                               */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento                                        */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
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
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  tip_trs                   varchar (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza che ha emesso il documento          */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento : s.aa.xx.nnnnnn                     */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  num_doc                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio per il movimento                        */
/*       *                                                       */
/*       * - N : No tipo archivio                                */
/*       * - C : Cliente                                         */
/*       * - F : Fornitore                                       */
/*       * - D : Deposito                                        */
/*       * - A : Agente (Incaricato alla vendita)                */
/*       *                                                       */
/*       * N.B. : Se il documento interessa la fatturazione, il  */
/*       *        tipo archivio deve essere pari a 'C'           */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza                                     */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice lingua per il documento                        */
/*       *                                                       */
/*       * N.B.: Da testata documento                            */
/*       *-------------------------------------------------------*/
  cod_lng                   varchar (03)                not null,
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
  sgl_vpf                   varchar (03)                not null,
  dec_vpf                   tinyint (01)                not null,
  tdc_vpf                   varchar (01)                not null,
  cdc_vpf                   decimal (12,05)             not null,
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
  tip_rig                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Prodotti di vendita    (Riga tipo 'P' o 'N')   */
/*       * - 02 : Semilavorati           (Riga tipo 'S')         */
/*       * - 03 : Materie prime          (Riga tipo 'M')         */
/*       * - 04 : Materiale vario        (Riga tipo 'V')         */
/*       *-------------------------------------------------------*/
  tip_mag                   tinyint (02)                not null,
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
  num_mag                   integer (07)                not null,
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
  alf_mag                   varchar (20)                not null,
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
/*       *                                                       */
/*       * N.B. : Utilizzato per numero e data lotto             */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  sgl_vrn                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto per il cliente                        */
/*       *-------------------------------------------------------*/
  cop_scl                   varchar (20)                not null,
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
/*       *           lettazione [bix]                            */
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
  des_ext                   tinyint (01)                not null,
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
  des_rig                   varchar (80)                not null,
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
  tip_pro                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per la vendita                             */
/*       *-------------------------------------------------------*/
  cod_iva                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per la vendita                   */
/*       *-------------------------------------------------------*/
  ctp_ven                   integer (07)                not null,
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
  umi_ven                   varchar (03)                not null,
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
  dec_qta                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' di vendita                                  */
/*       *                                                       */
/*       * N.B. : Solo per i tipi riga P,M,V,PN                  */
/*       *-------------------------------------------------------*/
  qta_ven                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione effettivo, da tipo movimento      */
/*       *-------------------------------------------------------*/
  cod_dsl                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della seconda quantita' nella     */
/*       * fatturazione                                          */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *-------------------------------------------------------*/
  snx_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la seconda quantita'              */
/*       *                                                       */
/*       * N.B. : Significativi solo se 'si' gestione seconda    */
/*       *        quantita'                                      */
/*       *-------------------------------------------------------*/
  dec_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Seconda quantita'                                     */
/*       *                                                       */
/*       * N.B. : Significativi solo se 'si' gestione seconda    */
/*       *        quantita'                                      */
/*       *-------------------------------------------------------*/
  qta_a02                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della terza quantita' nella       */
/*       * fatturazione                                          */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la terza quantita'                */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_3qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Terza quantita'                                       */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  qta_a03                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per il prezzo di vendita              */
/*       *-------------------------------------------------------*/
  dec_prz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per l'espressione dei prezzi standard          */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vps                   varchar (03)                not null,
  dec_vps                   tinyint (01)                not null,
  tdc_vps                   varchar (01)                not null,
  cdc_vps                   decimal (12,05)             not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo lordo standard, in valuta di espressione dei   */
/*       * prezzi standard                                       */
/*       *-------------------------------------------------------*/
  prz_lrs                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo netto standard, in valuta di espressione dei   */
/*       * prezzi standard : pari al prezzo lordo standard e-    */
/*       * ventualmente abbattuto dagli sconti standard          */
/*       *-------------------------------------------------------*/
  prz_nts                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per l'espressione del prezzo di vendita        */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vpp                   varchar (03)                not null,
  dec_vpp                   tinyint (01)                not null,
  tdc_vpp                   varchar (01)                not null,
  cdc_vpp                   decimal (12,05)             not null,
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
  flg_puq                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo unitario di vendita, in valuta                 */
/*       *-------------------------------------------------------*/
  prz_ven                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione del secondo prezzo nella fattura- */
/*       * zione                                                 */
/*       *                                                       */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_2pz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali del secondo prezzo                    */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_2pz                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Secondo prezzo, in valuta                             */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  prz_a02                   bigint  (11)                not null,
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
  sgl_vpl                   varchar (03)                not null,
  dec_vpl                   tinyint (01)                not null,
  tdc_vpl                   varchar (01)                not null,
  prz_vpl                   bigint  (11)                not null,
  cdc_vpl                   decimal (12,05)             not null,
  ccr_vpl                   decimal (12,05)             not null,
  plm_vpl                   decimal (05,02)             not null,
  tlm_vpl                   varchar (01)                not null,
  map_vpl                   varchar (01)                not null,
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
  epz_rgf                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria di sconto in riga associata al prodotto     */
/*       *-------------------------------------------------------*/
  csr_aap                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Precentuali di sconto in riga associate al prodotto   */
/*       *-------------------------------------------------------*/
  psr_ap1                   decimal (05,02)             not null,
  psr_ap2                   decimal (05,02)             not null,
  psr_ap3                   decimal (05,02)             not null,
  psr_ap4                   decimal (05,02)             not null,
  psr_ap5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Percentuali di sconto in riga 1..5                    */
/*       *-------------------------------------------------------*/
  per_sr1                   decimal (05,02)             not null,
  per_sr2                   decimal (05,02)             not null,
  per_sr3                   decimal (05,02)             not null,
  per_sr4                   decimal (05,02)             not null,
  per_sr5                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Prezzo netto effettivo, in valuta                     */
/*       *-------------------------------------------------------*/
  prz_net                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di esposizione prezzi e sconti della riga di     */
/*       * fattura                                               */
/*       *                                                       */
/*       * - Se tipo riga P o PN                                 */
/*       *                                                       */
/*       *    - 11 : Prezzo di vendita e sconti                  */
/*       *    - 21 : Solo prezzo netto                           */
/*       *-------------------------------------------------------*/
  epz_pes                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta per l'espressione del costo di riferimento     */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vpc                   varchar (03)                not null,
  dec_vpc                   tinyint (01)                not null,
  tdc_vpc                   varchar (01)                not null,
  cdc_vpc                   decimal (12,05)             not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per il costo di riferimento : 0..2    */
/*       *-------------------------------------------------------*/
  dec_cos                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Costo di riferimento, in valuta                       */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  cos_rif                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo definitivo per la riga, in valuta             */
/*       *-------------------------------------------------------*/
  imp_rig                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo ausiliario per gestione blocchi, in valuta    */
/*       *-------------------------------------------------------*/
  iau_rig                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria di provvigione associata al prodotto        */
/*       *-------------------------------------------------------*/
  cpv_aap                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % di provvigioni associate al prodotto                */
/*       *-------------------------------------------------------*/
  ppv_ap1                   decimal (05,02)             not null,
  ppv_ap2                   decimal (05,02)             not null,
  ppv_ap3                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Flag di significativita' dati provvigionali in riga   */
/*       *                                                       */
/*       * - 01 : Si, calcolate automaticamente                  */
/*       * - 02 : No provvigioni in riga                         */
/*       * - 03 : Si, speciali                                   */
/*       *-------------------------------------------------------*/
  fsp_rig                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Categoria di provvigione in riga effettiva            */
/*       *-------------------------------------------------------*/
  cpv_rig                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % di provvigioni in riga effettive                    */
/*       *-------------------------------------------------------*/
  ppv_ri1                   decimal (05,02)             not null,
  ppv_ri2                   decimal (05,02)             not null,
  ppv_ri3                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Importo provvigione a forfait in riga, in valuta base */
/*       *-------------------------------------------------------*/
  pvf_rig                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di riferimento ordine del cliente                */
/*       *-------------------------------------------------------*/
  ocl_dat                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero di riferimento ordine del cliente              */
/*       *-------------------------------------------------------*/
  ocl_num                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento nostra commissione al cliente          */
/*       *-------------------------------------------------------*/
  cmc_tip                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di riferimento nostra commissione al cliente     */
/*       *-------------------------------------------------------*/
  cmc_dat                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero di riferimento nostra commissione al cliente   */
/*       *-------------------------------------------------------*/
  cmc_num                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo documento nostra conferma d'ordine               */
/*       *-------------------------------------------------------*/
  coc_tip                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di riferimento nostra conferma d'ordine          */
/*       *-------------------------------------------------------*/
  coc_dat                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero di riferimento nostra conferma d'ordine        */
/*       *-------------------------------------------------------*/
  coc_num                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo nostra conferma d'ordine            */
/*       *-------------------------------------------------------*/
  coc_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo nel protocollo nostra conferma     */
/*       * d'ordine                                              */
/*       *-------------------------------------------------------*/
  coc_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di forzatura a saldo della quantita' di eva-  */
/*       * sione riga ordine cliente                             */
/*       *                                                       */
/*       * - spaces : No forzatura                               */
/*       * - S      : Forzatura a saldo                          */
/*       *-------------------------------------------------------*/
  coc_fzs                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo ordine di spedizione                */
/*       *-------------------------------------------------------*/
  ods_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo nel protocollo ordine spedizione   */
/*       *-------------------------------------------------------*/
  ods_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Documento da sottoporre a fatturazione differita      */
/*       *                                                       */
/*       * - N : No                                              */
/*       * - S : Si                                              */
/*       *-------------------------------------------------------*/
  fat_snx                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Data fattura                                          */
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
/*       * Flags di elaborazione bloccanti                       */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_bl1                   tinyint (01)                not null,
  flg_bl2                   tinyint (01)                not null,
  flg_bl3                   tinyint (01)                not null,
  flg_bl4                   tinyint (01)                not null,
  flg_bl5                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di elaborazione NON bloccanti                   */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   tinyint (01)                not null,
  flg_nb2                   tinyint (01)                not null,
  flg_nb3                   tinyint (01)                not null,
  flg_nb4                   tinyint (01)                not null,
  flg_nb5                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di riga verificata                               */
/*       *                                                       */
/*       * - 0 : Verificata                                      */
/*       * - 1 : Da verificare                                   */
/*       *-------------------------------------------------------*/
  flg_ver                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di riga esclusa dalla fatturazione               */
/*       *                                                       */
/*       * - 0 : nessuna esclusione                              */
/*       * - 1 : esclusa dalla fatturazione                      */
/*       *-------------------------------------------------------*/
  flg_esf                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di controllo in fase di consegna                 */
/*       *                                                       */
/*       * - 0 : riga non ancora consegnata                      */
/*       * - 1 : riga consegnata                                 */
/*       *-------------------------------------------------------*/
  flg_spn                   tinyint (01)                not null
