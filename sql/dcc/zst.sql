/*    *==========================================================*/
/*    * Tabella [zst] Codici statistici clienti            {dcc} */
/*    *                                                          */
/*    * Aggiornamento 22.04.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_cls,
                                                        cod_cls),
/*       *=======================================================*/
/*       * Tipo classificatore statistico                        */
/*       *                                                       */
/*       * - 01 : Zone classificazione clienti                   */
/*       * - 02 : Categorie classificazione clienti              */
/*       * - 03 : Codice statistico libero per classificazione   */
/*       *        clienti                                        */
/*       *-------------------------------------------------------*/
  tip_cls     tinyint (02)  not null COMMENT "Tipo"            ,
/*       *-------------------------------------------------------*/
/*       * Codice classificatore                                 */
/*       *-------------------------------------------------------*/
  cod_cls     integer (05)  not null COMMENT "Codice"          ,
/*       *-------------------------------------------------------*/
/*       * Descrizione classificatore                            */
/*       *-------------------------------------------------------*/
  des_cls     varchar (40)  not null COMMENT "Descrizione"     ,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico classificatore                       */
/*       *-------------------------------------------------------*/
  mne_cls     varchar (05)  not null COMMENT "Mnemonico"
