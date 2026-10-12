/*    *==========================================================*/
/*    * Tabella [uac] Utenti - Aziende - Comandi           {xpg} */
/*    *                                                          */
/*    * Aggiornamento 08.09.2025                                 */
/*    *----------------------------------------------------------*/


CREATE TABLE xpg800_folder (
  cod_abl   VARCHAR(6)  NOT NULL,       -- codice logico (es. DOCF01)
  des_folder VARCHAR(80) NOT NULL,      -- descrizione leggibile
  parent     VARCHAR(6)  DEFAULT NULL,  -- eventuale cartella padre
  path       VARCHAR(255) DEFAULT NULL, -- path fisico o logico
  sort       SMALLINT DEFAULT 0,        -- ordinamento
  attivo     TINYINT(1) DEFAULT 1,      -- 1=visibile, 0=nascosta
  PRIMARY KEY (cod_abl)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;



/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_ute,
                                                        cod_azi,
                                                        cod_abl),
/*       *=======================================================*/
/*       * Codice utente                                         */
/*       *-------------------------------------------------------*/
  cod_ute                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice azienda                                        */
/*       *-------------------------------------------------------*/
  cod_azi                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice elemento abilitazione                          */
/*       *-------------------------------------------------------*/
  cod_abl                   varchar  (06)               not null,
/*       *-------------------------------------------------------*/
/*       * Tipo abilitazione                                     */
/*       *                                                       */
/*       * -  +  : Abilitato                                     */
/*       * -  -  : Non abilitato                                 */
/*       * -  v  : Visualizzazione solo                          */
/*       *-------------------------------------------------------*/
  snv_abl                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Livello di abilitazione                               */
/*       *                                                       */
/*       *  Possibili definizioni        Esempi              Old */
/*       *  --------------------------- ----------           --- */
/*       *  -  0  : Sistema applicativo (swd,pgm,[su misura])(X) */
/*       *  -  1  : Gruppo gestionale   (ge,am,ve)           (G) */
/*       *  -  2  : Area gestionale     (xpg,cge)            (A) */
/*       *  -  3  : Settore gestionale  (cgemov)             (S) */
/*       *  -  4  : Fase gestionale     (cge300)             (F) */
/*       *-------------------------------------------------------*/
  liv_abl                   tinyint  (01)               not null
