/*    *==========================================================*/
/*    * Tabella [zvo] Causali operazioni veicoli                 */
/*    *                                                          */
/*    * Aggiornamento 03.09.2026                                 */
/*    *                                                          */
/*    * Decodifica e regole di trattamento del campo [tip_rig]   */
/*    * dell'archivio schede veicoli [gvs].                      */
/*    *                                                          */
/*    * N.B.: finche' azi200 e' in esercizio i codici sono       */
/*    *       cablati nel programma COBOL: il contenuto di       */
/*    *       questa tabella non puo' essere esteso.             */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_ope),
/*       *=======================================================*/
/*       * Codice operazione                                     */
/*       *                                                       */
/*       * Corrisponde a gvs.tip_rig                             */
/*       *-------------------------------------------------------*/
  cod_ope                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione                                           */
/*       *-------------------------------------------------------*/
  des_ope                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione breve per griglia                         */
/*       *-------------------------------------------------------*/
  des_brv                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero d'ordine di presentazione                      */
/*       *-------------------------------------------------------*/
  num_ord                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no causale valida                                  */
/*       *                                                       */
/*       * Le causali non piu' utilizzabili si disattivano,      */
/*       * non si cancellano: lo storico deve restare            */
/*       * decodificabile.                                       */
/*       *-------------------------------------------------------*/
  snx_val                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Segno movimento nel calcolo dei costi                 */
/*       *                                                       */
/*       *  - '+' : costo (acquisto, manutenzione, tasse)        */
/*       *  - '-' : ricavo (vendita)                             */
/*       *  - 'N' : non concorre                                 */
/*       *-------------------------------------------------------*/
  sgn_mov                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio atteso su gvs.tip_arc                   */
/*       *                                                       */
/*       *  - 'N' : Nessuno                                      */
/*       *  - 'F' : Fornitori                                    */
/*       *  - 'C' : Clienti                                      */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Requisiti sui campi della riga operazione             */
/*       *                                                       */
/*       *  - 'O' : obbligatorio                                 */
/*       *  - 'F' : facoltativo                                  */
/*       *  - 'N' : non applicabile                              */
/*       *                                                       */
/*       * rqs_doc : data e numero documento                     */
/*       * rqs_imp : importo documento                           */
/*       * rqs_utl : lettura utilizzo (km oppure ore)            */
/*       * rqs_scd : data scadenza                               */
/*       * rqs_not : note operazione                             */
/*       *-------------------------------------------------------*/
  rqs_doc                   varchar (01)                not null,
  rqs_imp                   varchar (01)                not null,
  rqs_utl                   varchar (01)                not null,
  rqs_scd                   varchar (01)                not null,
  rqs_not                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no l'operazione mette il veicolo in carico         */
/*       *-------------------------------------------------------*/
  snx_acq                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no l'operazione dismette il veicolo                */
/*       *-------------------------------------------------------*/
  snx_dsm                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/no promemoria automatico proposto di default       */
/*       *-------------------------------------------------------*/
  snx_pma                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Giorni di preavviso del promemoria                    */
/*       *-------------------------------------------------------*/
  gio_pma                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null
