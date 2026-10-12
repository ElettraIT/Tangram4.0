/*    *==========================================================*/
/*    * Tabella [zsx] Sconti incrociati                    {dcc} */
/*    *                                                          */
/*    * Adattata per Sito Web TheCreativeWay                     */
/*    *                                                          */
/*    * Aggiornamento 19.11.2012                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_uno,
                                                        cod_due),
/*       *-------------------------------------------------------*/
/*       * 1. codice per l'incrocio                              */
/*       *                                                       */
/*       * - codice categoria di sconto in riga associato al     */
/*       *   cliente                                             */
/*       *-------------------------------------------------------*/
  cod_uno                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * 2. codice per l'incrocio                              */
/*       *                                                       */
/*       * - codice categoria di sconto associato al prodotto    */
/*       *-------------------------------------------------------*/
  cod_due                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * % di sconto associate all'incrocio                    */
/*       *-------------------------------------------------------*/
  per_sc1                   decimal (05,02)             not null,
  per_sc2                   decimal (05,02)             not null,
  per_sc3                   decimal (05,02)             not null,
  per_sc4                   decimal (05,02)             not null,
  per_sc5                   decimal (05,02)             not null