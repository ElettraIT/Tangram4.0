/*    *==========================================================*/
/*    * Tabella [ddp] Distinte di presentazione            {gep} */
/*    *                                                          */
/*    * Aggiornamento 22.10.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_ddp),
/*       *=======================================================*/
/*       * Numero distinta : s.aa.nnnnnnnn                       */
/*       *-------------------------------------------------------*/
  num_ddp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di composizione distinta */
/*       *-------------------------------------------------------*/
  dtr_com                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo distinta                                         */
/*       *                                                       */
/*       * - 01 : Incassi elettronici                            */
/*       * - 02 : Effetti                                        */
/*       * - 03 : Paghero' cambiari                              */
/*       * - 04 : Cessioni da clienti                            */
/*       *-------------------------------------------------------*/
  tip_ddp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo avviso richiesto                                 */
/*       *                                                       */
/*       * - 01 : Conferme d'ordine oppure Ri.Ba.                */
/*       * - 02 : Conferme d'ordine oppure M.Av.                 */
/*       * - 03 : Solo Ri.Ba.                                    */
/*       * - 04 : Solo M.Av.                                     */
/*       * - 05 : Solo R.I.D.                                    */
/*       *-------------------------------------------------------*/
  tav_ric                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipi scadenza da includere in distinta                */
/*       *                                                       */
/*       * - 0002 : Incassi elettronici                          */
/*       * - 0003 : Ri.Ba.                                       */
/*       * - 0004 : C.d.O.                                       */
/*       * - 0005 : M.Av.                                        */
/*       * - 0006 : R.I.D.                                       */
/*       * - 0009 : Ricevute bancarie                            */
/*       * - 0010 : Tratte                                       */
/*       * - 0011 : Paghero' cambiari                            */
/*       * - 0061 : Paghero' cambiari avuti in cessione          */
/*       * - 0203 : Incassi elettronici e Ri.Ba.                 */
/*       * - 0204 : Incassi elettronici e C.d.O.                 */
/*       * - 0205 : Incassi elettronici e M.Av.                  */
/*       * - 0910 : Ricevute bancarie e Tratte                   */
/*       *-------------------------------------------------------*/
  tsc_das                   integer (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Data scadenza minima da selezionare                   */
/*       *-------------------------------------------------------*/
  dsc_min                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data scadenza massima da selezionare                  */
/*       *-------------------------------------------------------*/
  dsc_max                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Anche scadenze a vista da selezionare                 */
/*       *                                                       */
/*       * - S : Si                                              */
/*       * - N : No                                              *
/*       *-------------------------------------------------------*/
  dsc_vis                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Selezione su circuiti interbancari                    */
/*       *                                                       */
/*       * N.B.: (01/06/98) Dal momento che questa selezione non */
/*       *                  viene piu' utilizzata, la si utiliz- */
/*       *                  za per ospitare il campo 'ns. banca  */
/*       *                  di appoggio' il quale opera una se-  */
/*       *                  lezione a livello di anagrafica com- */
/*       *                  merciale del cliente.                */
/*       *                                                       */
/*       *                  Il nuovo campo occupa tutti e 10 i   */
/*       *                  caratteri disponibili.               */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  sel_ban                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo massimo per la distinta, in valuta base       */
/*       *-------------------------------------------------------*/
  imp_max                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo effettivo della distinta, in valuta base      */
/*       *-------------------------------------------------------*/
  imp_ddp                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di presentazione distin- */
/*       * ta alla banca                                         */
/*       *-------------------------------------------------------*/
  dtr_pre                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di presentazione distinta alla banca    */
/*       *-------------------------------------------------------*/
  drc_pre                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di presentazione distinta alla banca    */
/*       *-------------------------------------------------------*/
  npc_pre                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di presentazione                                 */
/*       *                                                       */
/*       * - 01 : Accredito SBF a maturazione di valuta          */
/*       * - 02 : Accredito immediato SBF                        */
/*       * - 03 : Accredito al dopo incasso                      */
/*       * - 04 : Allo sconto                                    */
/*       *-------------------------------------------------------*/
  tip_pre                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice nostra banca per la presentazione              */
/*       *-------------------------------------------------------*/
  cod_cbp                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Stampa cartacea distinta                              */
/*       * - S : Si                                              */
/*       * - N : No                                              */
/*       *-------------------------------------------------------*/
  snx_stp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag/contatore di stampa su carta eseguita, solo per  */
/*       * le stampe eseguite post presentazione distinta        */
/*       *-------------------------------------------------------*/
  flc_stp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Preparazione archivio di supporto per trasmissione    */
/*       * distinta alla banca                                   */
/*       * - S : Si                                              */
/*       * - N : No                                              */
/*       *-------------------------------------------------------*/
  snx_ads                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag/contatore di preparazione archivio di supporto   */
/*       * per trasmissione distinta alla banca eseguita         */
/*       *-------------------------------------------------------*/
  flc_ads                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di invio dell'archivio di supporto alla banca    */
/*       * - 01 : Tramite dischetto                              */
/*       * - 02 : Tramite Home Banking                           */
/*       *-------------------------------------------------------*/
  tip_ias                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag/contatore di invio dell'archivio di supporto al- */
/*       * la banca eseguito                                     */
/*       *-------------------------------------------------------*/
  flc_ias                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di accettazione distinta */
/*       * da parte della banca                                  */
/*       *-------------------------------------------------------*/
  dtr_act                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di accettazione distinta da parte della */
/*       * banca                                                 */
/*       *-------------------------------------------------------*/
  drc_act                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di accettazione distinta da parte della */
/*       * banca                                                 */
/*       *-------------------------------------------------------*/
  npc_act                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data contabile bancaria per il movimento di accetta-  */
/*       * zione distinta da parte della banca                   */
/*       *-------------------------------------------------------*/
  dcb_act                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero contabile bancaria per il movimento di accet-  */
/*       * tazione distinta da parte della banca                 */
/*       *-------------------------------------------------------*/
  ncb_act                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese addebitate dalla banca al momento della */
/*       * accettazione della distinta, in valuta base           */
/*       *-------------------------------------------------------*/
  spe_act                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo interessi addebitati dalla banca al momento   */
/*       * della accettazione della distinta, in valuta base     */
/*       *-------------------------------------------------------*/
  int_act                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di accredito distinta in */
/*       * conto corrente da parte della banca                   */
/*       *-------------------------------------------------------*/
  dtr_acd                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione movimento di contabilita' a fronte */
/*       * del movimento di accredito distinta in conto corrente */
/*       * da parte della banca                                  */
/*       *-------------------------------------------------------*/
  drc_acd                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di contabilita' a fronte  */
/*       * del movimento di accredito distinta in conto corrente */
/*       * da parte della banca                                  */
/*       *-------------------------------------------------------*/
  npc_acd                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data contabile bancaria per il movimento di accredito */
/*       * distinta in conto corrente da parte della banca       */
/*       *-------------------------------------------------------*/
  dcb_acd                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero contabile bancaria per il movimento di accre-  */
/*       * dito distinta in conto corrente da parte della banca  */
/*       *-------------------------------------------------------*/
  ncb_acd                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo spese addebitate dalla banca al momento del-  */
/*       * l'accredito in conto corrente da parte della banca,   */
/*       * in valuta base                                        */
/*       *-------------------------------------------------------*/
  spe_acd                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo interessi addebitati dalla banca al momento   */
/*       * dell'accredito in conto corrente da parte della banca */
/*       * in valuta base                                        */
/*       *-------------------------------------------------------*/
  int_acd                   bigint  (13)                not null,
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
