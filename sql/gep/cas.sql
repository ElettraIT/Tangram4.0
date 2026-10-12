/*    *==========================================================*/
/*    * Tabella [cas] Casse                                {gep} */
/*    *                                                          */
/*    * Aggiornamento 29.03.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cas),
/*       *=======================================================*/
/*       * Codice nostra cassa                                   */
/*       *-------------------------------------------------------*/
  cod_cas                   varchar (10)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione nostra cassa                              */
/*       *-------------------------------------------------------*/
  des_cas                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per la cassa contanti            */
/*       *-------------------------------------------------------*/
  stc_csc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per la cassa assegni             */
/*       *                                                       */
/*       * Nota: Questo sottoconto non viene mai utilizzato.     */
/*       *                                                       */
/*       *       Anche l'operazione di riscossione per assegno,  */
/*       *       della gestione portafoglio, utilizzera' il      */
/*       *       sottoconto precedente.                          */
/*       *                                                       */
/*       *       L'utente che volesse tenere una cassa assegni   */
/*       *       separata dalla cassa contanti non dovra' far    */
/*       *       altro che istituire due anagrafiche cassa.      */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  stc_csa                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile per oneri su pagamenti per cassa */
/*       *                                                       */
/*       * Nota: Per quanto riguarda la cassa viene sempre       */
/*       *       utilizzato questo sottoconto.                   */
/*       *-------------------------------------------------------*/
  stc_opc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Percorso immagine                                     */
/*       *-------------------------------------------------------*/
  pth_img                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
