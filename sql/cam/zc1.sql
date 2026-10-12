/*    *==========================================================*/
/*    * Tabella [zc1] Classi cespiti ammortizzabili              */
/*    *                                                          */
/*    * Aggiornamento 02.09.2026                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cla),
/*       *=======================================================*/
/*       * Codice                                                */
/*       *-------------------------------------------------------*/
  cod_cla                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione                                           */
/*       *-------------------------------------------------------*/
  des_cla                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Mnemonico                                             */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Sottoconto contabile                                  */
/*       *-------------------------------------------------------*/
  pdc_cla                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * % di ammortamento di default                          */
/*       *-------------------------------------------------------*/
  per_amr                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * % di deducibilita' fiscale di default                 */
/*       *                                                       */
/*       * N.B.: campo aggiunto in fase di migrazione Symfony,   */
/*       *       non presente nel tracciato COBOL originale.     */
/*       *       NULL = 100% (deducibilita' piena)                */
/*       *-------------------------------------------------------*/
  per_ded                   decimal (05,02)             null,
/*       *-------------------------------------------------------*/
/*       * Tetto di costo fiscalmente rilevante di default        */
/*       * (in centesimi)                                         */
/*       *                                                       */
/*       * N.B.: campo aggiunto in fase di migrazione Symfony,   */
/*       *       non presente nel tracciato COBOL originale.     */
/*       *       NULL = nessun tetto                              */
/*       *-------------------------------------------------------*/
  tdc_fsc                   bigint                      null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
