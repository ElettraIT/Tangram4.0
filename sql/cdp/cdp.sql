/*    *==========================================================*/
/*    * Tabella [cdp] Movimenti commesse produzione        {cdp} */
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
  tmo_cdp                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra dipendenza                              */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento : s.aa.xx.nnnnnn                     */
/*       *                                                       */
/*       * - s      : secolo                                     */
/*       * - aa     : anno                                       */
/*       * - xx     : codice dipendenza                          */
/*       * - nnnnnn : numero progressivo                         */
/*       *-------------------------------------------------------*/
  num_doc                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Secolo ed anno relativi alla data documento           */
/*       *-------------------------------------------------------*/
  scl_ann                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla numerazione relativa al numero documento        */
/*       *-------------------------------------------------------*/
  sgl_num                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Destinazione della produzione                         */
/*       * - 01 : Per il magazzino                               */
/*       * - 02 : Per un cliente                                 */
/*       *-------------------------------------------------------*/
  tip_dsp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice per la destinazione della produzione           */
/*       * - Se destinazione 01 : Per il magazzino               */
/*       *   - Zero                                              */
/*       * - Se destinazione 02 : Per un cliente                 */
/*       *   - Codice cliente                                    */
/*       *-------------------------------------------------------*/
  cod_dsp                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza per la destinazione della produ-    */
/*       * zione                                                 */
/*       * - Se destinazione 01 : Per il magazzino               */
/*       *   - Spaces                                            */
/*       * - Se destinazione 02 : Per un cliente                 */
/*       *   - Codice dipendenza del cliente                     */
/*       *-------------------------------------------------------*/
  dpz_dsp                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Esecuzione della produzione                           */
/*       * - 01 : Lavorazione interna                            */
/*       * - 02 : Lavorazione esterna                            */
/*       *-------------------------------------------------------*/
  tip_esp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice per l'esecuzione della produzione              */
/*       * - Se esecuzione della produzione 01 : Interna         */
/*       *   - Zero                                              */
/*       * - Se esecuzione della produzione 02 : Esterna         */
/*       *   - Codice fornitore                                  */
/*       *-------------------------------------------------------*/
  cod_esp                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza per l'esecuzione della produzione   */
/*       * - Se esecuzione della produzione 01 : Interna         */
/*       *   - Spaces                                            */
/*       * - Se esecuzione della produzione 02 : Esterna         */
/*       *   - Codice dipendenza del fornitore                   */
/*       *-------------------------------------------------------*/
  dpz_esp                   varchar (07)                not null,
  cod_rsp                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data consegna prevista                                */
/*       *-------------------------------------------------------*/
  dat_cns                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Priorita' di produzione richiesta, ovvero grado di    */
/*       * urgenza da noi assegnato alla produzione              */
/*       *-------------------------------------------------------*/
  pri_prd                   varchar (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data consegna prevista                                */
/*       *-------------------------------------------------------*/
  com_int                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       * - 01 : Prodotto di vendita                            */
/*       * - 02 : Semilavorato                                   */
/*       *-------------------------------------------------------*/
  tip_mag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice numerico di magazzino                          */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice alfanumerico di magazzino                      */
/*       *-------------------------------------------------------*/
  alf_mag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Sigla della variante per il codice magazzino          */
/*       *                                                       */
/*       * N.B. : Campo attualmente non gestito                  */
/*       *-------------------------------------------------------*/
  sgl_vrn                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Unita' di misura per la produzione, da anagrafica     */
/*       *-------------------------------------------------------*/
  umi_prd                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali quantita' per la produzione : 0..3,   */
/*       * da anagrafica                                         */
/*       *-------------------------------------------------------*/
  dec_qta                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Quantita' da produrre                                 */
/*       *-------------------------------------------------------*/
  qta_dap                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Codice dislocazione effettivo                         */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  cod_dsl                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della seconda quantita' nella     */
/*       * produzione                                            */
/*       * - 0 : No                                              */
/*       * - 1 : Si                                              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  snx_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero decimali per la seconda quantita'              */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  dec_2qt                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Seconda quantita'                                     */
/*       *                                                       */
/*       * N.B. : Questo campo non e' gestito                    */
/*       *-------------------------------------------------------*/
  qta_a02                   decimal (13,03)             not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di gestione della terza quantita' nella       */
/*       * produzione                                            */
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
/*       * Numero di stampe eseguite, max 99                     */
/*       *-------------------------------------------------------*/
  ctr_stp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di commessa comunque considerata saldata      */
/*       * - Spaces : Non considerata saldata                    */
/*       * - #      : Considerata saldata                        */
/*       *                                                       */
/*       * Nota : Questo flag viene manipolato durante la fase   */
/*       *        di versamento dalla produzione.                */
/*       *-------------------------------------------------------*/
  sdc_ccs                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di commessa completamente chiusa                 */
/*       *                                                       */
/*       * - Spaces : Non ancora chiusa                          */
/*       * - #      : Completamente chiusa                       */
/*       *-------------------------------------------------------*/
  flg_cch                   varchar (01)                not null,
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
