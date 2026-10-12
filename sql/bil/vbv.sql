/*    *==========================================================*/
/*    * Tabella [vbv] Valori di bilancio                         */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_bil,
                                                        dat_bil,
                                                        vdb_lv1,
                                                        vdb_lv2,
                                                        vdb_lv3,
                                                        vdb_lv4,
                                                        vdb_lv5,
                                                        vdb_lv6),
/*       *=======================================================*/
/*       * Tipo di bilancio interessato per il valore            */
/*       *-------------------------------------------------------*/
  tip_bil                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di riferimento bilancio                          */
/*       *-------------------------------------------------------*/
  dat_bil                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Codice voce di bilancio di livello 1                  */
/*       *-------------------------------------------------------*/
  vdb_lv1                   integer (05)                not null,
  vdb_lv2                   integer (05)                not null,
  vdb_lv3                   integer (05)                not null,
  vdb_lv4                   integer (05)                not null,
  vdb_lv5                   integer (05)                not null,
  vdb_lv6                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Valore voce di bilancio                               */
/*       *-------------------------------------------------------*/
  val_vdb                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
