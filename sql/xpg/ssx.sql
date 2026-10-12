/*    *==========================================================*/
/*    * Tabella [ssx] Sessioni utente - processi figli     {xpg} */
/*    *                                                          */
/*    * Aggiornamento 23.06.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (idx_aut),
/*       *=======================================================*/
/*       * ID numerico automatico                                */
/*       *-------------------------------------------------------*/
  aut_ssx                   integer               AUTO_INCREMENT,
/*       *-------------------------------------------------------*/
/*       * ID di sessione                                        */
/*       *-------------------------------------------------------*/
  ide_ssx                   varchar  (32)               not null,
/*       *-------------------------------------------------------*/
/*       * window token                                          */
/*       *-------------------------------------------------------*/
  wtk_ssx                   varchar  (64)               not null,
/*       *-------------------------------------------------------*/
/*       * Data creazione                                        */
/*       *-------------------------------------------------------*/
  din_ssx              date NOT NULL DEFAULT current_timestamp(),
/*       *-------------------------------------------------------*/
/*       * Data ultimo accesso                                   */
/*       *-------------------------------------------------------*/
  lad_ssx              date NOT NULL DEFAULT current_timestamp(),
/*       *-------------------------------------------------------*/
/*       * Chiave esterna                                        */
/*       *-------------------------------------------------------*/
    FOREIGN KEY (idx_ssn) REFERENCES ssn(idx_ssn) ON DELETE CASCADE,
    UNIQUE KEY (idx_ssn, window_token)

/*
    CREATE TABLE ssx (
      aut_ssx INT AUTO_INCREMENT PRIMARY KEY,
      ide_ssx VARCHAR(32) NOT NULL,
      wtk_ssx VARCHAR(64) NOT NULL,
      din_ssx DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
      lad_ssx DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      FOREIGN KEY (ide_ssx) REFERENCES ssn(idx_ssn) ON DELETE CASCADE,
      UNIQUE KEY (ide_ssx, wtk_ssx)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
*/
  
