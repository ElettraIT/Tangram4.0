/*    *==========================================================*/
/*    * Tabella [lic] Lettere d'intento dei clienti        {cge} */
/*    *                                                          */
/*    * Aggiornamento 04.10.2026                                 */
/*    *                                                          */
/*    * Piu' lettere per cliente (progressivo): la fattura usa   */
/*    * quelle valide alla sua data (nel periodo, non revocate), */
/*    * in ordine di progressivo, finche' hanno importo residuo. */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_cli,
                                                        prg_lic),
/*       *=======================================================*/
/*       * Codice cliente                                        */
/*       *-------------------------------------------------------*/
  cod_cli                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo della lettera per il cliente (1, 2, ...)  */
/*       *                                                       */
/*       * N.B. : nell'import dal vecchio ERP vale 1             */
/*       *-------------------------------------------------------*/
  prg_lic                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di lettera                                       */
/*       *                                                       */
/*       * - 01 : Fino a concorrenza dell'importo (imp_plf)      */
/*       * - 02 : Singola operazione                             */
/*       *-------------------------------------------------------*/
  tip_lic                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero lettera d'intenti (del cliente)                */
/*       *-------------------------------------------------------*/
  num_lic                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Data lettera d'intenti                                */
/*       *-------------------------------------------------------*/
  dat_lic                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Protocollo di ricezione dell'Agenzia delle entrate    */
/*       *                                                       */
/*       * N.B. : va riportato nella fattura elettronica         */
/*       *-------------------------------------------------------*/
  prt_ade                   varchar (30)                not null,
/*       *-------------------------------------------------------*/
/*       * Data del protocollo dell'Agenzia delle entrate        */
/*       *-------------------------------------------------------*/
  dat_ade                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Numero protocollazione interno (registro delle        */
/*       * dichiarazioni ricevute)                               */
/*       *-------------------------------------------------------*/
  prt_int                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Periodo di validita': data iniziale                   */
/*       *-------------------------------------------------------*/
  drf_ini                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Periodo di validita': data finale                     */
/*       *-------------------------------------------------------*/
  drf_fin                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Importo fino a concorrenza (plafond), in valuta base, */
/*       * centesimi                                             */
/*       *                                                       */
/*       * N.B. : zero = nessun limite indicato                  */
/*       *-------------------------------------------------------*/
  imp_plf                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di revoca della lettera                          */
/*       *                                                       */
/*       * N.B. : 1900-01-01 = non revocata                      */
/*       *-------------------------------------------------------*/
  dat_rev                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Testo per la fattura                                  */
/*       *-------------------------------------------------------*/
  prn_txt                   varchar (120)               not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
