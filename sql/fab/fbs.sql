/*    *==========================================================*/
/*    * Tabella [fbs] Scorte di magazzino                  {fab} */
/*    *                                                          */
/*    * Dipendenza zero = tutte (???)                            */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_mag,
                                                        num_mag,
                                                        var_mag,
                                                        cod_dpz),
/*       *=======================================================*/
/*       * Codice nostra dipendenza                              */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo codice di magazzino                              */
/*       *                                                       */
/*       * - 01 : Prodotti di vendita                            */
/*       * - 03 : Materie prime                                  */
/*       * - 04 : Materiali vari                                 */
/*       *-------------------------------------------------------*/
  tip_mag                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice prodotto numerico                              */
/*       *-------------------------------------------------------*/
  num_mag                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Variante di magazzino                                 */
/*       *-------------------------------------------------------*/
  var_mag                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo stagionalita'                                    */
/*       *-------------------------------------------------------*/
  tip_stg                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Parametro di scorta minima 1                          */
/*       *                                                       */
/*       * - Scorta minima                                       */
/*       * - Data ultimo aggiornamento scorta minima             */
/*       * - Scorta di sicurezza                                 */
/*       * - Data ultimo aggiornamento scorta di sicurezza       */
/*       * - Scorta massima                                      */
/*       * - Data ultimo aggiornamento scorta massima            */
/*       *-------------------------------------------------------*/
  sco_mi1                   decimal (11,03)             not null,
  dua_mi1                   date                        not null,
  sco_si1                   decimal (11,03)             not null,
  dua_si1                   date                        not null,
  sco_ma1                   decimal (11,03)             not null,
  dua_ma1                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Parametro di scorta minima 2                          */
/*       *                                                       */
/*       * - Scorta minima                                       */
/*       * - Data ultimo aggiornamento scorta minima             */
/*       * - Scorta di sicurezza                                 */
/*       * - Data ultimo aggiornamento scorta di sicurezza       */
/*       * - Scorta massima                                      */
/*       * - Data ultimo aggiornamento scorta massima            */
/*       *-------------------------------------------------------*/
  sco_mi2                   decimal (11,03)             not null,
  dua_mi2                   date                        not null,
  sco_si2                   decimal (11,03)             not null,
  dua_si2                   date                        not null,
  sco_ma2                   decimal (11,03)             not null,
  dua_ma2                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Parametro di scorta minima 3                          */
/*       *                                                       */
/*       * - Scorta minima                                       */
/*       * - Data ultimo aggiornamento scorta minima             */
/*       * - Scorta di sicurezza                                 */
/*       * - Data ultimo aggiornamento scorta di sicurezza       */
/*       * - Scorta massima                                      */
/*       * - Data ultimo aggiornamento scorta massima            */
/*       *-------------------------------------------------------*/
  sco_mi3                   decimal (11,03)             not null,
  dua_mi3                   date                        not null,
  sco_si3                   decimal (11,03)             not null,
  dua_si3                   date                        not null,
  sco_ma3                   decimal (11,03)             not null,
  dua_ma3                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Parametro di scorta minima 4                          */
/*       *                                                       */
/*       * - Scorta minima                                       */
/*       * - Data ultimo aggiornamento scorta minima             */
/*       * - Scorta di sicurezza                                 */
/*       * - Data ultimo aggiornamento scorta di sicurezza       */
/*       * - Scorta massima                                      */
/*       * - Data ultimo aggiornamento scorta massima            */
/*       *-------------------------------------------------------*/
  sco_mi4                   decimal (11,03)             not null,
  dua_mi4                   date                        not null,
  sco_si4                   decimal (11,03)             not null,
  dua_si4                   date                        not null,
  sco_ma4                   decimal (11,03)             not null,
  dua_ma4                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Annotazioni                                           */
/*       *-------------------------------------------------------*/
  not_sco                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
