/*    *==========================================================*/
/*    * Tabella [job] Coda lavori asincroni                {xpg} */
/*    *                                                          */
/*    * Aggiornamento 15.09.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                           (cod_job),
/* */
  KEY                       JOB_STA_AVA                 (sts_job,
                                                         dat_ava),
/* */
  KEY                       JOB_TIP_STS                 (tip_job,
                                                         sts_job),
/* */
  KEY                       JOB_DAT_INS                 (dat_ins),
/*       *=======================================================*/
/*       * Numero coda                                           */
/*       *-------------------------------------------------------*/
  cod_job                   bigint  (11) NOT NULL AUTO_INCREMENT,
/*       *-------------------------------------------------------*/
/*       * Tipo job / codice funzione                            */
/*       *                                                       */
/*       * Esempi:                                               */
/*       *  - FAB410_PDF                                         */
/*       *  - FAT_BOLLE                                          */
/*       *  - ecc.                                               */
/*       *-------------------------------------------------------*/
  tip_job                   varchar(40)                 not null,
/*       *-------------------------------------------------------*/
/*       * Stato job                                             */
/*       *                                                       */
/*       *  - N : Nuovo (in coda)                                */
/*       *  - R : In esecuzione                                  */
/*       *  - C : Completato                                     */
/*       *  - E : Errore                                         */
/*       *  - X : Annullato                                      */
/*       *-------------------------------------------------------*/
  sts_job                   varchar(1)       NOT NULL DEFAULT 'N',
/*       *-------------------------------------------------------*/
/*       * Priorità di esecuzione (più alto = più urgente)       */
/*       *-------------------------------------------------------*/
  pri_job                   tinyint(2)       NOT NULL DEFAULT 0,
/*       *-------------------------------------------------------*/
/*       * Percentuale avanzamento 0..100                        */
/*       *-------------------------------------------------------*/
  prg_prc                   tinyint(3)       NOT NULL DEFAULT 0,
/*       *-------------------------------------------------------*/
/*       * Messaggio sintetico stato / errore                    */
/*       *-------------------------------------------------------*/
  msg_log                   varchar(255)     NOT NULL DEFAULT '',
/*       *-------------------------------------------------------*/
/*       * Payload parametri in formato JSON                     */
/*       *                                                       */
/*       * Contiene i criteri di elaborazione:                   */
/*       *  - dat_ela, cod_dpz, tip_mag, filter_id, ecc.         */
/*       *-------------------------------------------------------*/
  pay_jsn                   longtext                    NOT NULL,
/*       *-------------------------------------------------------*/
/*       * Risultato in JSON (opzionale)                         */
/*       *                                                       */
/*       * Es: lista righe pre-elaborate, statistiche, ecc.      */
/*       *-------------------------------------------------------*/
  ris_jsn                   longtext                        NULL,
/*       *-------------------------------------------------------*/
/*       * Percorso file generato (es. PDF)                      */
/*       *-------------------------------------------------------*/
  fil_out                   varchar(255)      NOT NULL DEFAULT '',

/*       *-------------------------------------------------------*/
/*       * Conteggio tentativi / errori                          */
/*       *-------------------------------------------------------*/
  num_err                   int(3)             NOT NULL DEFAULT 0,
  num_rip                   tinyint(2)         NOT NULL DEFAULT 0,
/*       *-------------------------------------------------------*/
/*       * Timestamp inserimento job                             */
/*       *-------------------------------------------------------*/
  dat_ins                   datetime                     NOT NULL,
/*       *-------------------------------------------------------*/
/*       * Job eseguibile NON prima di questa data/ora           */
/*       * (di solito = dat_ins, ma puoi schedulare in futuro)   */
/*       *-------------------------------------------------------*/
  dat_ava                   datetime                     NOT NULL,
/*       *-------------------------------------------------------*/
/*       * Timestamp inizio e fine esecuzione                    */
/*       *-------------------------------------------------------*/
  dat_sta                   datetime                         NULL,
  dat_fin                   datetime                         NULL,
/*       *-------------------------------------------------------*/
/*       * Identificativo utente / origine job                   */
/*       *-------------------------------------------------------*/
  ute_ins                   varchar(32)        NOT NULL DEFAULT ''
