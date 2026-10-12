/*    *==========================================================*/
/*    * Tabella [crm] Gestione documentale                 {azi} */
/*    *                                                          */
/*    * Aggiornamento 21.06.2025                                 */
/*    *                                                          */
/*    * ___ IN LAVORAZIONE ___                                   */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (sgl_are,
                                                        cod_mod,
                                                        cod_lng,
                                                        cod_lbl),
/*       *=======================================================*/
/*       * Tipo documento di riferimento                         */
/*       *                                                       */
/*       * - 01 : Ordine cliente                         Vendite */
/*       * - 11 : Ordine di spedizione                   Vendite */
/*       * - 21 : Documento di trasporto                 Vendite */
/*       * - 31 : Fattura di vendita                     Vendite */
/*       * - 41 : Scadenza cliente ___ ATTUALMENTE NON GESTITE   */
/*       *-------------------------------------------------------*/
  tip_crm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativo documento sorgente                     */
/*       *-------------------------------------------------------*/
  prt_crm                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativo documento sorgente - riga              */
/*       *-------------------------------------------------------*/
  prg_crm                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice tipo evento                                    */
/*       *-------------------------------------------------------*/
  cod_crm                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Commento per l'evento                                 */
/*       *-------------------------------------------------------*/
  com_crm                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
