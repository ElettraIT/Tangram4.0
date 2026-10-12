/*    *==========================================================*/
/*    * Tabella [ocx] Ordini fornitori                     {orf} */
/*    *                                                          */
/*    * Estensione a testate e righe                             */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        num_prg,
                                                        tip_rec),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo riga                                      */
/*       *-------------------------------------------------------*/
  num_prg                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo record                                           */
/*       *                                                       */
/*       * - 01 : Estensioni alla testata (Solo se progressivo a */
/*       *                                 zero)                 */
/*       * - 11 : Descrizione alternativa a quella normale in    */
/*       *        riga bolla                                     */
/*       *-------------------------------------------------------*/
  tip_rec                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Scomposizione in 10 righe                             */
/*       *-------------------------------------------------------*/
  val_r01                   varchar (40)                not null,
  val_r02                   varchar (40)                not null,
  val_r03                   varchar (40)                not null,
  val_r04                   varchar (40)                not null,
  val_r05                   varchar (40)                not null,
  val_r06                   varchar (40)                not null,
  val_r07                   varchar (40)                not null,
  val_r08                   varchar (40)                not null,
  val_r09                   varchar (40)                not null,
  val_r10                   varchar (40)                not null
