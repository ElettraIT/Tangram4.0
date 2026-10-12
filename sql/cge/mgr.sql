/*    *==========================================================*/
/*    * Tabella [mgr] Primanota : righe                          */
/*    *                                                          */
/*    * N.B.: eliminata la data registrazione dalla chiave       */
/*    *                                                          */
/*    * Aggiornamento 18.02.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
/*       *                                                       */
/*       * Indici : "PDCDAT    " = rf-mgr-cod-pdc                */
/*       *                         rf-mgr-dat-reg                */
/*       *                         rf-mgr-num-prt                */
/*       *                         rf-mgr-num-prg                */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        num_prg),
/* */
  KEY                       PDCDAT                      (cod_pdc,
                                                         dat_reg,
                                                         num_prt,
                                                         num_prg),
/*       *=======================================================*/
/*       * Numero protocollo di contabilita' generale            */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo entro il movimento                 */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di registrazione di contabilita' generale        */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice sottoconto contabile                           */
/*       *-------------------------------------------------------*/
  cod_pdc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio                                         */
/*       *                                                       */
/*       * - G = generale                                        */
/*       * - C = clienti                                         */
/*       * - F = fornitori                                       */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio se tipo 'C' o 'F'                     */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice causale di contabilita'                        */
/*       *-------------------------------------------------------*/
  cod_cau                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale si/no movimento di bilancio della causale     */
/*       *                                                       */
/*       * - S : Si, movimento di rettifica al bilancio          */
/*       * - N : No, movimento di contabilita' ordinaria         */
/*       * - X : Movimento di chiusura e apertura bilancio       */
/*       *-------------------------------------------------------*/
  snx_mob                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo movimento iva                                    */
/*       *                                                       */
/*       * - 1 : Dare  cliente Italia o Estero non CEE           */
/*       * - 2 : Avere cliente Italia o Estero non CEE           */
/*       * - 3 : Corrispettivi Italia o Estero non CEE           */
/*       *                                                       */
/*       * - 4 : Dare  fornitore Italia o Estero non CEE         */
/*       * - 5 : Avere fornitore Italia o Estero non CEE         */
/*       * - 6 : Bolla doganale  Estero non CEE                  */
/*       *                                                       */
/*       * - A : Dare  cliente Estero CEE                        */
/*       * - B : Avere cliente Estero CEE                        */
/*       *                                                       */
/*       * - D : Dare  fornitore Estero CEE                      */
/*       * - E : Avere fornitore Estero CEE                      */
/*       *-------------------------------------------------------*/
  tip_iva                   varchar (01)                    null,
/*       *-------------------------------------------------------*/
/*       * Commento a livello di riga                            */
/*       *-------------------------------------------------------*/
  com_rig                   varchar (160)               not null,
/*       *-------------------------------------------------------*/
/*       * Data documento                                        */
/*       *-------------------------------------------------------*/
  dat_doc                   date                            null,
/*       *-------------------------------------------------------*/
/*       * Numero documento                                      */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Data riferimento                                      */
/*       *-------------------------------------------------------*/
  dat_rif                   date                            null,
/*       *-------------------------------------------------------*/
/*       * Numero riferimento                                    */
/*       *-------------------------------------------------------*/
  num_rif                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag dare/avere                                       */
/*       *                                                       */
/*       * - D = Dare                                            */
/*       * - A = Avere                                           */
/*       *-------------------------------------------------------*/
  dar_ave                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo movimento                                     */
/*       *-------------------------------------------------------*/
  imp_mov                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di avvenuta stampa partitario di generale        */
/*       *-------------------------------------------------------*/
  flg_pge                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flag di avvenuta stampa partitario clienti/fornitori  */
/*       *-------------------------------------------------------*/
  flg_pcf                   tinyint (01)                not null
