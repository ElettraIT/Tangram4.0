/*    *==========================================================*/
/*    * Tabella [zcs] Categorie sconti clienti             {dcc} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_csc,
                                                        cod_csc),
/*       *=======================================================*/
/*       * Tipo categoria di sconto                              */
/*       *                                                       */
/*       * - 01 : Categorie sconto in chiusura legate al cliente */
/*       * - 02 : Categorie sconto in riga legate al cliente     */
/*       * - 03 : Categorie sconto in riga legate al prodotto    */
/*       *-------------------------------------------------------*/
  tip_csc                   tinyint (02)                not null,
/*       *=======================================================*/
/*       * Codice categoria di sconto                            */
/*       *-------------------------------------------------------*/
  cod_csc                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione categoria di sconto                       */
/*       *-------------------------------------------------------*/
  des_csc                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico categoria di sconto                  */
/*       *-------------------------------------------------------*/
  mne_csc                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * % di sconto associate direttamente alla categoria di  */
/*       * sconto                                                */
/*       *-------------------------------------------------------*/
  prc_sc1                   decimal (05,02)             not null,
  prc_sc2                   decimal (05,02)             not null,
  prc_sc3                   decimal (05,02)             not null,
  prc_sc4                   decimal (05,02)             not null,
  prc_sc5                   decimal (05,02)             not null