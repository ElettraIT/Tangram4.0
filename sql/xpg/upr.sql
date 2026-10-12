/*    *==========================================================*/
/*    * Tabella [upr] User Personal Reminder               {xpg} */
/*    *                                                          */
/*    * Aggiornamento 27.08.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_ute,
                                                        cod_upr),
/*       *=======================================================*/
/*       * Codice utente                                         */
/*       *-------------------------------------------------------*/
  cod_ute                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Codice promemoria                                     */
/*       *-------------------------------------------------------*/
  cod_upr                   integer  (05)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione promemoria                                */
/*       *-------------------------------------------------------*/
  des_upr                   varchar  (40)               not null,
/*       *-------------------------------------------------------*/
/*       * Testo promemoria                                      */
/*       *-------------------------------------------------------*/
  txt_upr                   text                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di promemoria                                    */
/*       *                                                       */
/*       *  - '01' : Scadenza amministrativa                     */
/*       *  - '02' : Promemoria importante                       */
/*       *  - '03' : Scadenza periodica                          */
/*       *  - '04' : ____                                        */
/*       *                                                       */
/*       *  N.B.: per differenziare colore nota                  */
/*       *-------------------------------------------------------*/
  tip_upr                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di ripetizione                              */
/*       *                                                       */
/*       *  - '01' : Solo alla data indicata, no ripetizione     */
/*       *  - '02' : Lo stesso giorno di ogni settimana          */
/*       *  - '03' : Lo stesso giorno di ogni mese               */
/*       *  - '04' : Lo stesso giorno di ogni anno               */
/*       *  - '05' : Ogni giorno                                 */
/*       *-------------------------------------------------------*/
  mdr_upr                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di avviso                                   */
/*       *                                                       */
/*       *  - '01' : Al primo collegamento del giorno            */
/*       *  - '02' : Ad ogni collegamento del giorno             */
/*       *  - '03' : Ad ogni cambio di programma o menu'         */
/*       *-------------------------------------------------------*/
  mda_upr                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Data di riferimento                                   */
/*       *                                                       */
/*       * N.B.: solo se modalita' di ripetizione '01'           */
/*       *-------------------------------------------------------*/
  dat_upr                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Giorno della settimana di riferimento                 */
/*       *                                                       */
/*       * N.B.: solo se modalita' di ripetizione '02'           */
/*       *-------------------------------------------------------*/
  gio_set                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Giorno del mese di riferimento                        */
/*       *                                                       */
/*       * N.B.: solo se modalita' di ripetizione '03' o '04'    */
/*       *-------------------------------------------------------*/
  gio_rif                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Mese di riferimento                                   */
/*       *                                                       */
/*       * N.B.: solo se modalita' di ripetizione '04'           */
/*       *-------------------------------------------------------*/
  mes_rif                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Numero giorni di preavviso                            */
/*       *                                                       */
/*       * N.B.: max 10                                          */
/*       *-------------------------------------------------------*/
  ngp_upr                   integer  (03)               not null,
/*       *-------------------------------------------------------*/
/*       * Modalita' di preavviso                                */
/*       *                                                       */
/*       *  - '01' : nessun preavviso                            */
/*       *  - '02' : unico preavviso                             */
/*       *  - '03' : quotidiano                                  */
/*       *-------------------------------------------------------*/
  mdp_upr                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Durata promemoria                                     */
/*       *                                                       */
/*       *  - '01' : Nessun limite di durata                     */
/*       *  - '02' : Fino alla data indicata                     */
/*       *                                                       */
/*       * N.B.: Se modalita' di ripetizione '01', forzatura a   */
/*       *       '02'                                            */
/*       *-------------------------------------------------------*/
  drt_upr                   tinyint  (02)               not null,
/*       *-------------------------------------------------------*/
/*       * Data finale promemoria                                */
/*       *-------------------------------------------------------*/
  dfi_upr                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Data ultima presa visione                             */
/*       *-------------------------------------------------------*/
  dpv_upr                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Si/no auto cancellazione promemoria scaduto           */
/*       *                                                       */
/*       *  - 0 : No                                             */
/*       *  - 1 : Si                                             */
/*       *-------------------------------------------------------*/
  snx_acp                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no promemoria visionato                            */
/*       *                                                       */
/*       *  - 0 : No                                             */
/*       *  - 1 : Si                                             */
/*       *                                                       */
/*       * N.B.: campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  snx_pvs                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no collegamento a scadenziario passivo             */
/*       *                                                       */
/*       *  - 0 : No                                             */
/*       *  - 1 : Si                                             */
/*       *                                                       */
/*       * N.B.: campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  snx_scf                   tinyint  (01)               not null
/*       *-------------------------------------------------------*/
/*       * Preferenza colore se diversa da standard              */
/*       * ___ DA IMPLEMENTARE ___                               */
/*       *-------------------------------------------------------*/
/*col_upr                   varchar  (08)               not null */
