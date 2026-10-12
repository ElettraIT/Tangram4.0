/*    *==========================================================*/
/*    * Tabella [smp] Parametri di invio mail              {xpg} */
/*    *                                                          */
/*    * Aggiornamento 23.01.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_smp),
/*       *=======================================================*/
/*       * Tipo invio                                            */
/*       *                                                       */
/*       * - 01 : Invio conferme ordini clienti            (orc) */
/*       * - 11 : Invio ordini di spedizione clienti       (ods) */
/*       * - 21 : Invio bolle clienti                      (bol) */
/*       * - 31 : Invio fatture XML clienti                (fat) */
/*       * - 32 : Invio fatture PDF clienti                (fat) */
/*       * - 51 : Invio solleciti a clienti                (gep) */
/*       *-------------------------------------------------------*/
  tip_smp                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di invio                                    */
/*       *                                                       */
/*       * - 01 : Una mail per ciascun file                      */
/*       * - 02 : Una mail con un unico file ZIP                 */
/*       *                                                       */
/*       * N.B.: tipo '02' solo se Invio fatture XML clienti     */
/*       *-------------------------------------------------------*/
  mod_smp                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Percorso file ZIP                                     */
/*       *                                                       */
/*       * N.B.: solo se Invio fatture XML clienti               */
/*       *-------------------------------------------------------*/
  dir_zip                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Parametri SMTP - Host, porta e cifratura              */
/*       *-------------------------------------------------------*/
  hst_smp                   varchar (255)               not null,
  prt_smp                   smallint                    not null,
  enc_smp                   tinyint                     not null,
/*       *-------------------------------------------------------*/
/*       * Parametri SMTP - Username                             */
/*       *-------------------------------------------------------*/
  usr_smp                   varchar (255)               not null,
/*       *-------------------------------------------------------*/
/*       * Parametri SMTP - Password                             */
/*       *-------------------------------------------------------*/
  pwd_smp                   text                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di email                                         */
/*       *                                                       */
/*       * - 01 : Normale                                        */
/*       * - 02 : PEC                                            */
/*       *-------------------------------------------------------*/
  tip_eml                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Email mittente                                        */
/*       *-------------------------------------------------------*/
  emt_smp                   varchar (255)               not null,
/*       *-------------------------------------------------------*/
/*       * Email mittente, nominativo                            */
/*       *-------------------------------------------------------*/
  nmt_smp                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       * Email mittente, Reply-To                              */
/*       *-------------------------------------------------------*/
  rpl_smp                   varchar (255)               not null,
/*       *-------------------------------------------------------*/
/*       * Email in copia (opzionale)                            */
/*       *-------------------------------------------------------*/
  emc_smp                   varchar (255)               not null,
/*       *-------------------------------------------------------*/
/*       * Email destinatario SDI                                */
/*       *                                                       */
/*       * N.B.: solo se Invio fatture XML clienti               */
/*       *-------------------------------------------------------*/
  sdi_smp                   varchar (255)               not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (12)                not null,
  ide_fas                   varchar (06)                not null
