/*    *==========================================================*/
/*    * Tabella [tst] Tipo stampante                       {xpg} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_tst),
/*       *=======================================================*/
/*       * Codice tipo stampante                                 */
/*       *-------------------------------------------------------*/
  cod_tst                   varchar  (08)               not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo stampante                            */
/*       *-------------------------------------------------------*/
  des_tst                   varchar  (80)               not null,
/*       *-------------------------------------------------------*/
/*       * Driver per il tipo stampante                          */
/*       *-------------------------------------------------------*/
  drv_tst                   varchar  (20)               not null,
/*       *-------------------------------------------------------*/
/*       * Larghezza carrello in 72mi di pollice                 */
/*       *-------------------------------------------------------*/
  l72_tst                   integer  (04)               not null,
/*       *-------------------------------------------------------*/
/*       * Altezza carrello in 72mi di pollice                   */
/*       *-------------------------------------------------------*/
  a72_tst                   integer  (04)               not null,
/*       *-------------------------------------------------------*/
/*       * Margine in 72mi di pollice                            */
/*       *                                                       */
/*       * - Superiore                                           */
/*       * - Inferiore                                           */
/*       * - Sinistro                                            */
/*       * - Destro                                              */
/*       *-------------------------------------------------------*/
  msu_tst                   integer  (04)               not null,
  min_tst                   integer  (04)               not null,
  msi_tst                   integer  (04)               not null,
  mde_tst                   integer  (04)               not null,
/*       *-------------------------------------------------------*/
/*       * Spaziature orizzontali                                */
/*       *-------------------------------------------------------*/
  spa_or1                   decimal  (05,02)            not null,
  spa_or2                   decimal  (05,02)            not null,
  spa_or3                   decimal  (05,02)            not null,
  spa_or4                   decimal  (05,02)            not null,
  spa_or5                   decimal  (05,02)            not null,
  spa_or6                   decimal  (05,02)            not null,
/*       *-------------------------------------------------------*/
/*       * Spaziature verticali                                  */
/*       *-------------------------------------------------------*/
  spa_ve1                   decimal  (05,02)            not null,
  spa_ve2                   decimal  (05,02)            not null,
  spa_ve3                   decimal  (05,02)            not null,
/*       *-------------------------------------------------------*/
/*       * Classe di stampante                                   */
/*       *                                                       */
/*       * - Spaces : Normale                                    */
/*       * - PS     : PostScript                                 */
/*       * - PCL5   : PCL5                                       */
/*       * - SPPX   : Special Printer                            */
/*       * - FAX    : Fax (con software di interfaccia)          */
/*       *-------------------------------------------------------*/
  cla_tst                   varchar  (10)               not null,
/*       *-------------------------------------------------------*/
/*       * Sottoclasse di stampante                              */
/*       *                                                       */
/*       * - Se classe Spaces                                    */
/*       *   - Spaces                                            */
/*       *                                                       */
/*       * - Se classe PS                                        */
/*       *   - A4   :Formato A4                                  */
/*       *   - A3   :Formato A3                                  */
/*       *   - A2   :Formato A2                                  */
/*       *   - A1   :Formato A1                                  */
/*       *   - A0   :Formato A0                                  */
/*       *                                                       */
/*       * - Se classe PCL5                                      */
/*       *   - A4   :Formato A4                                  */
/*       *   - A3   :Formato A3                                  */
/*       *   - A2   :Formato A2                                  */
/*       *   - A1   :Formato A1                                  */
/*       *   - A0   :Formato A0                                  */
/*       *                                                       */
/*       * - Se classe SPPX                                      */
/*       *   - Spaces                                            */
/*       *                                                       */
/*       * - Se classe ZFAX                                      */
/*       *   - Spaces                                            */
/*       *-------------------------------------------------------*/
  scl_tst                   varchar  (10)               not null,
/*       *-------------------------------------------------------*/
/*       * Parametri per il driver di stampa                     */
/*       *-------------------------------------------------------*/
  par_pd1                   varchar  (10)               not null,
  par_pd2                   varchar  (10)               not null,
  par_pd3                   varchar  (10)               not null,
  par_pd4                   varchar  (10)               not null,
  par_pd5                   varchar  (10)               not null,
  par_pd6                   varchar  (10)               not null,
/*       *-------------------------------------------------------*/
/*       * Non-printable area in 72mi di pollice                 */
/*       *                                                       */
/*       * - Superiore                                           */
/*       * - Inferiore                                           */
/*       * - Sinistra                                            */
/*       * - Destra                                              */
/*       *-------------------------------------------------------*/
  nsu_tst                   integer  (04)               not null,
  nin_tst                   integer  (04)               not null,
  nsi_tst                   integer  (04)               not null,
  nde_tst                   integer  (04)               not null,
/*       *-------------------------------------------------------*/
/*       * Classe di appartenenza                                */
/*       *-------------------------------------------------------*/
  cla_app                   varchar  (01)               not null,
/*       *-------------------------------------------------------*/
/*       * Sistema applicativo di appartenenza                   */
/*       *-------------------------------------------------------*/
  sap_app                   varchar  (04)               not null,
/*       *-------------------------------------------------------*/
/*       * Percorso immagine                                     */
/*       *-------------------------------------------------------*/
  pth_img                   varchar  (80)               not null
