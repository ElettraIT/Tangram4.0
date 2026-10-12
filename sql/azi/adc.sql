/*    *==========================================================*/
/*    * Tabella [adc] Dati azienda - contatti                    */
/*    *                                                          */
/*    * Aggiornamento 14.08.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (tip_arc,
                                                        cod_arc,
                                                        dpz_arc,
                                                        prg_arc),
/*       *=======================================================*/
/*       * Tipo archivio                                         */
/*       *                                                       */
/*       * - 00 : Azienda                                        */
/*       * - 01 : Cliente   contabile                            */
/*       * - 02 : Cliente   commerciale                          */
/*       * - 03 : Cliente   marketing                            */
/*       * - 11 : Fornitore contabile                            */
/*       * - 12 : Fornitore commerciale                          */
/*       * - 21 : Dipendenza aziendale                           */
/*       * - 31 : Agente                                         */
/*       * - 41 : Vettore                                        */
/*       *-------------------------------------------------------*/
  tip_arc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice dipendenza archivio                            */
/*       *-------------------------------------------------------*/
  dpz_arc                   varchar (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero progressivo di registrazione                   */
/*       *-------------------------------------------------------*/
  prg_arc                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo contatto                                         */
/*       *                                                       */
/*       * - TEL : Telefono                                      */
/*       * - FAX : Fax                                           */
/*       * - CEL : Cellulare                                     */
/*       * - EML : E-mail                                        */
/*       * - PEC : E-mail PEC                                    */
/*       * - WWW : Sito Internet                                 */
/*       * - SEP : Separatore per visualizzazione                */
/*       *-------------------------------------------------------*/
  tip_con                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Nome interlocutore                                    */
/*       *-------------------------------------------------------*/
  int_con                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Reparto interlocutore                                 */
/*       *                                                       */
/*       * - [fat] : Per invio fatture                           */
/*       * - [bol] : Per invio bolle                             */
/*       * - [ord] : Per invio ordini                            */
/*       *-------------------------------------------------------*/
  rep_con                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Utenza contatto                                       */
/*       *                                                       */
/*       * Prefisso internazionale                               */
/*       *                                                       */
/*       * N.B.: Campo attualmente non gestito                   */
/*       *-------------------------------------------------------*/
  pri_con                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Utenza contatto                                       */
/*       *                                                       */
/*       * Prefisso nazionale                                    */
/*       *-------------------------------------------------------*/
  pre_con                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Utenza contatto                                       */
/*       *                                                       */
/*       * Numero o identificativo o indirizzo                   */
/*       *-------------------------------------------------------*/
  num_con                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Data aggiornamento                                    */
/*       *-------------------------------------------------------*/
  dat_agg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
