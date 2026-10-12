/*    *==========================================================*/
/*    * Tabella [gpm] Maturazioni provvigionali                  */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_mtz),
/*       *-------------------------------------------------------*/
/*       * Chiavi addizionali                                    */
/*       *-------------------------------------------------------*/
  INDEX                     CTGMTZ                     (num_ctg,
                                                        num_mtz),
/*       *=======================================================*/
/*       * Numero maturazione                                    */
/*       *-------------------------------------------------------*/
  num_mtz                   integer (09)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero conteggio provvigionale di riferimento         */
/*       *-------------------------------------------------------*/
  num_ctg                   integer (09)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di riferimento per la maturazione                */
/*       *                                                       */
/*       * N.B.: Questo valore non puo' mai mancare, in quanto   */
/*       *       costituisce la data su cui vengono parzializ-   */
/*       *       zate tutte le elaborazioni sull'archivio.       */
/*       *-------------------------------------------------------*/
  dat_mtz                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di maturazione                                   */
/*       *  - 01 : Maturazione immediata di provvigione          */
/*       *  - 02 : Maturazione a fronte incasso di provvigione   */
/*       *  - 51 : Storno di provvigione                         */
/*       *                                                       */
/*       * In caso di conteggio a fronte di una nota di accredi- */
/*       * to il tipo di maturazione e' sempre immediato.        */
/*       *                                                       */
/*       * Note sul tipo maturazione 51, di storno:              */
/*       *                                                       */
/*       *  - Non e' mai generato automaticamente, bensi' puo'   */
/*       *    solo essere impostato manualmente.                 */
/*       *                                                       */
/*       *  - Non deve essere confuso con una maturazione imme-  */
/*       *    diata di tipo positivo a fronte di una nota di     */
/*       *    accredito, che costituisce una detrazione. Infat-  */
/*       *    ti il tipo maturazione di storno puo' riferirsi    */
/*       *    sia ad un conteggio positivo sia ad un conteggio   */
/*       *    negativo. In entrambi i casi non modifica l'im-    */
/*       *    porto maturato totale per l'agente, bensi' serve   */
/*       *    solo per chiudere il conteggio.                    */
/*       *-------------------------------------------------------*/
  tip_mtz                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo incassato, che fa' scattare la maturazione    */
/*       * proporzionalmente all'importo totale del documento    */
/*       * cui si riferisce il conteggio, eventualmente aumen-   */
/*       * tato di eventuali acconti fatturati                   */
/*       *                                                       */
/*       * N.B.: Solo se tipo di maturazione 02                  */
/*       *-------------------------------------------------------*/
  imp_inc                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo di provvigione maturato                       */
/*       *-------------------------------------------------------*/
  imp_mtz                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Annotazioni sulla maturazione                         */
/*       *-------------------------------------------------------*/
  not_mtz                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags bloccanti                                       */
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
/*       *-------------------------------------------------------*/
  flg_nb1                   varchar (01)                not null,
  flg_nb2                   varchar (01)                not null,
  flg_nb3                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia                                      */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
