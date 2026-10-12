/*    *==========================================================*/
/*    * Tabella [tmp] Dati utente temporanei               {xpg} */
/*    *                                                          */
/*    * Aggiornamento 22.06.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (idx_ssn,
                                                        tbl_nam,
                                                        rcd_key),
/*       *=======================================================*/
/*       * ID di sessione                                        */
/*       *-------------------------------------------------------*/
  idx_ssn                   varchar  (32)               not null,
/*       *-------------------------------------------------------*/
/*       * Nome tabella                                          */
/*       *-------------------------------------------------------*/
  tbl_nam                   varchar  (10)               not null,
/*       *-------------------------------------------------------*/
/*       * Chiave                                                */
/*       *-------------------------------------------------------*/
  rcd_key                   varchar  (64)               not null,
/*       *-------------------------------------------------------*/
/*       * Dati JSON                                             */
/*       *-------------------------------------------------------*/
  dat_jsn                   longtext                    not null,
/*       *-------------------------------------------------------*/
/*       * Data                                                  */
/*       *-------------------------------------------------------*/
  ide_dat         timestamp NOT NULL DEFAULT current_timestamp()



CREATE TABLE tmp (
  idx_ssn VARCHAR(32) NOT NULL,
  tbl_nam VARCHAR(10) NOT NULL,
  rcd_key VARCHAR(64) NOT NULL,
  dat_jsn LONGTEXT NOT NULL,
  ide_dat TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (idx_ssn, tbl_nam, rcd_key)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
