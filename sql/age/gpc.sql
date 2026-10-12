/*    *==========================================================*/
/*    * Tabella [gpc] Conteggi provvigionali                     */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_ctg),
/*       *=======================================================*/
/*       * Numero conteggio provvigionale di riferimento         */
/*       *-------------------------------------------------------*/
  num_ctg                   integer (09)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollo movimento di fatture clienti        */
/*       *                                                       */
/*       * N.B.: Solo se conteggio generato automaticamente dal- */
/*       *       la procedura di fatturazione.                   */
/*       *       Servira' in caso di maturazione legata agli     */
/*       *       incassi a fronte del documento.                 */
/*       *                                                       */
/*       *       Se questo numero di protocollo manca, significa */
/*       *       che l'imputazione e' stata manuale.             */
/*       *-------------------------------------------------------*/
  prt_fcl                   integer (09)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di conteggio                                     */
/*       *                                                       */
/*       *  - 01 : Conteggio a fronte fattura                    */
/*       *  - 02 : Conteggio a fronte nota di addebito           */
/*       *  - 03 : Conteggio a fronte nota di accredito          */
/*       *  - 11 : Conteggio a fronte fattura, ma in detrazione  */
/*       *-------------------------------------------------------*/
  tip_ctg                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo vendita per l'agente                             */
/*       *  - 01 : Diretta                                       */
/*       *  - 02 : Indiretta                                     */
/*       *-------------------------------------------------------*/
  tip_vpa                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data documento cui si riferisce il conteggio          */
/*       *                                                       */
/*       * N.B.: Questo valore non puo' mai mancare, in quanto   */
/*       *       costituisce la data su cui vengono parzializ-   */
/*       *       zate tutte le elaborazioni sull'archivio.       */
/*       *-------------------------------------------------------*/
  dat_doc                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero documento cui si riferisce il conteggio, alli- */
/*       * neato a destra, per avere un ordinamento              */
/*       *-------------------------------------------------------*/
  num_doc                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente                                         */
/*       *-------------------------------------------------------*/
  cod_age                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice cliente                                        */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza cliente                             */
/*       *-------------------------------------------------------*/
  dpz_cli                   varchar (04)                not null,
/*       *-------------------------------------------------------*/
/*       * Imponibile provvigionale in valuta base               */
/*       *-------------------------------------------------------*/
  ibl_pvg                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * % di provvigione                                      */
/*       *                                                       */
/*       * Nota : Possono esistere conteggi senza provvigione, e */
/*       *        in questo caso sia la percentuale che l'ammon- */
/*       *        tare sono a zero                               */
/*       *-------------------------------------------------------*/
  per_pvg                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Ammontare provvigione conteggiata sul documento, in   */
/*       * valuta base                                           */
/*       *                                                       */
/*       * Nota : Possono esistere conteggi senza provvigione, e */
/*       *        in questo caso sia la percentuale che l'ammon- */
/*       *        tare sono a zero                               */
/*       *-------------------------------------------------------*/
  amm_pvg                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo totale del documento cui si riferisce il con- */
/*       * teggio, in valuta base                                */
/*       *-------------------------------------------------------*/
  imp_doc                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo totale di eventuali acconti gia' fatturati    */
/*       * assorbiti nel documento cui si riferisce il conteg-   */
/*       * gio, in valuta base                                   */
/*       *-------------------------------------------------------*/
  imp_agf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Annotazioni sul conteggio provvigionale               */
/*       *-------------------------------------------------------*/
  not_ctg                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di maturazione prevista per la provvigione       */
/*       *                                                       */
/*       * - 01 : Maturazione immediata                          */
/*       * - 02 : Maturazione su incassi a fronte documento      */
/*       *                                                       */
/*       * N.B. : In caso di conteggio a fronte di una nota di   */
/*       *        accredito questo campo viene forzato al valo-  */
/*       *        re 01.                                         */
/*       *-------------------------------------------------------*/
  tip_mat                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Segnale di provvigione con maturazione bloccata       */
/*       *                                                       */
/*       * - 01 : Maturazione della provvigione non bloccata     */
/*       * - 02 : Maturazione della provvigione bloccata         */
/*       *                                                       */
/*       * N.B. : In caso di conteggio a fronte di una nota di   */
/*       *        accredito questo campo viene forzato al valo-  */
/*       *        re 01.                                         */
/*       *-------------------------------------------------------*/
  mat_blo                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di maturazione minima per la provvigione         */
/*       *                                                       */
/*       * N.B. : In caso di valore a zero si intende che la da- */
/*       *        ta di maturazione minima sia pari alla data    */
/*       *        del documento (maturazione a vista)            */
/*       *                                                       */
/*       * N.B. : In caso di conteggio a fronte di una nota di   */
/*       *        accredito questo campo viene forzato al valo-  */
/*       *        re zero.                                       */
/*       *-------------------------------------------------------*/
  ddm_min                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice agente subordinato                             */
/*       *                                                       */
/*       * N.B. : Solo se provvigione di un super-agente         */
/*       *-------------------------------------------------------*/
  cod_ags                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice del cliente per la fatturazione                */
/*       *                                                       */
/*       * N.B. : Puo' essere diverso dal codice cliente         */
/*       *        precedentemente citato                         */
/*       *-------------------------------------------------------*/
  cod_plf                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Dipendenza cliente per la fatturazione                */
/*       *                                                       */
/*       * N.B. : Puo' essere diverso dal codice cliente         */
/*       *        precedentemente citato                         */
/*       *-------------------------------------------------------*/
  dpz_plf                   varchar (04)                not null,
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
/*       * Flags di pulizia effettuabile                         */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

