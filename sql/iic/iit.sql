/*    *==========================================================*/
/*    * Tabella [iit] Iva Intracomunitaria : testate       {iic} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        dat_reg),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Data di registrazione                                 */
/*       *-------------------------------------------------------*/
  dat_reg                   date                        not null,
/*       *-------------------------------------------------------*/
/*       * Tipo operazione                                       */
/*       *                                                       */
/*       *  - 0001 : Acquisto                                    */
/*       *  - 0002 : Cessione                                    */
/*       *  - 0011 : Rettifica ad acquisto                       */
/*       *  - 0012 : Rettifica a cessione                        */
/*       *-------------------------------------------------------*/
  top_iic                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Data registrazione del movimento di contabilita' ge-  */
/*       * nerale connesso al movimento                          */
/*       *                                                       */
/*       * Puo' mancare                                          */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  dtr_cge                   date                        not null, 
/*       *-------------------------------------------------------*/
/*       * Numero protocollo del movimento di contabilita' ge-   */
/*       * nerale connesso al movimento                          */
/*       *                                                       */
/*       * Puo' mancare                                          */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  prt_cge                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo archivio                                         */
/*       *                                                       */
/*       *  - C : Cliente                                        */
/*       *  - F : Fornitore                                      */
/*       *                                                       */
/*       * Puo' mancare                                          */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  tip_arc                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio                                       */
/*       *                                                       */
/*       * E' il codice contabile del cliente o del fornitore    */
/*       *                                                       */
/*       * Puo' mancare                                          */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  cod_arc                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice archivio precedente                            */
/*       *                                                       */
/*       * E' il codice contabile del cliente o del fornitore    */
/*       * precedente                                            */
/*       *                                                       */
/*       * Puo' mancare                                          */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  arc_pre                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ISO dello Stato membro CEE cui l'operatore co- */
/*       * munitario appartiene                                  */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  iso_opc                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ISO dello Stato membro CEE cui l'operatore co- */
/*       * munitario appartiene precedente                       */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  iso_ocp                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice IVA dell'operatore comunitario                 */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  cdi_opi                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice IVA dell'operatore comunitario precedente      */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  cdi_ocp                   varchar (20)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice della natura della transazione                 */
/*       *                                                       */
/*       *  - 0 : Non definita                                   */
/*       *  - 1 : Acquisto o vendita, compreso il baratto        */
/*       *  - 2 : Restituzione o sostituzione di merci           */
/*       *  - 3 : Aiuti governativi, privati o finanziati dalla  */
/*       *        comunita' europea                              */
/*       *  - 4 : Operazione in vista di una lavorazione per     */
/*       *        conto terzi o di una riparazione               */
/*       *  - 5 : Operazione successiva ad una lavorazione per   */
/*       *        conto terzi o di una riparazione               */
/*       *  - 6 : Movimento di merci senza trasferimento di      */
/*       *        proprieta'                                     */
/*       *  - 7 : Operazione a titolo di un programma comune di  */
/*       *        difesa o di un altro programma intergovernati- */
/*       *        vo di fabbricazione coordinata                 */
/*       *  - 8 : Fornitura di materiali e macchinari nel quadro */
/*       *        di un contratto generale di costruzione o di   */
/*       *        genio civile                                   */
/*       *  - 9 : Altre transazioni                              */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per cessioni                                        */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  cdn_dtr                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice della natura della transazione precedente      */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per rettifiche ad acquisti                          */
/*       * - Per rettifiche a cessioni                           */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  cdn_dtp                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice del regime                                     */
/*       *                                                       */
/*       *  - 4 : Introduzione definitiva (immissione in consu-  */
/*       *        mo)                                            */
/*       *  - 5 : Introduzione temporanea a scopo di perfeziona- */
/*       *        mento (lavorazione, riparazione, trasformazio- */
/*       *        ne, ecc.)                                      */
/*       *  - 6 : Reintroduzione a seguito di spedizione tempo-  */
/*       *        ranea a scopo di perfezionamento               */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per cessioni                                        */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       *     N.B.: Campo obsoleto (D.M.04/02/98)               */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  cod_rgm                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice condizioni di consegna                         */
/*       *                                                       */
/*       *  - 00 : Non definita                                  */
/*       *                                                       */
/*       *  - 01 : Franco fabbrica                       EXW (E) */
/*       *                                                       */
/*       *  - 11 : Franco vettore                        FCA (F) */
/*       *  - 12 : Franco sotto bordo                    FAS (F) */
/*       *  - 13 : Franco a bordo                        FOB (F) */
/*       *                                                       */
/*       *  - 21 : Costo e nolo                          CFR (C) */
/*       *  - 22 : Costo, assicurazione, nolo            CIF (C) */
/*       *  - 23 : Nolo/porto pagato fino a ...          CPT (C) */
/*       *  - 24 : Nolo/porto e assic. pagati fino a ... CIP (C) */
/*       *                                                       */
/*       *  - 31 : Reso frontiera                        DAF (D) */
/*       *  - 32 : Reso franco bordo nave a destino      DES (D) */
/*       *  - 33 : Reso franco banchina                  DEQ (D) */
/*       *  - 34 : Reso non sdoganato                    DDU (D) */
/*       *  - 35 : Reso sdoganato                        DDP (D) */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per cessioni                                        */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       *     N.B.: Campo nuovo (D.M.04/02/98) - vedi prec.     */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  cod_cdc                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice del modo di trasporto                          */
/*       *                                                       */
/*       *  - 0 : Non definito                                   */
/*       *  - 1 : Trasporto marittimo                            */
/*       *  - 2 : Trasporto ferroviario                          */
/*       *  - 3 : Trasporto stradale                             */
/*       *  - 4 : Trasporto aereo                                */
/*       *  - 5 : Spedizioni postali                             */
/*       *  - 7 : Installazioni fisse di trasporto               */
/*       *  - 8 : Trasporto per vie d'acqua                      */
/*       *  - 9 : Propulsione propria                            */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per cessioni                                        */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  cod_mdt                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice ISO dello Stato membro CEE di provenienza o di */
/*       * destinazione delle merci                              */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti, paese di provenienza                  */
/*       * - Per cessioni, paese di destinazione                 */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  iso_pod                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice della provincia italiana di provenienza o di   */
/*       * destinazione delle merci                              */
/*       *                                                       */
/*       * Corrisponde alla sigla automobilistica                */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti, provincia di destinazione             */
/*       * - Per cessioni, provincia di origine                  */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  pvc_ood                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Totale del corrispettivo dell'operazione espresso in  */
/*       * valuta estera                                         */
/*       *                                                       */
/*       * Non significativo in caso di assenza della data di    */
/*       * registrazione e numero di protocollo di contabili-    */
/*       * ta' generale                                          */
/*       *                                                       */
/*       *     ---------------------------------------------     */
/*       *                                                       */
/*       * - Per acquisti                                        */
/*       * - Per rettifiche ad acquisti                          */
/*       *                                                       */
/*       *-------------------------------------------------------*/
  tco_eiv                   bigint  (13)                not null,
/*       *-------------------------------------------------------*/
/*       * Valuta                                                */
/*       *                                                       */
/*       * - Sigla                                               */
/*       * - Decimali                                            */
/*       * - Tipo cambio                                         */
/*       * - Cambio                                              */
/*       *-------------------------------------------------------*/
  sgl_vpf                   varchar (03)                not null,
  dec_vpf                   tinyint (01)                not null,
  tdc_vpf                   varchar (01)                not null,
  cdc_vpf                   decimal (12,05)             not null,
/*       *-------------------------------------------------------*/
/*       * Flags bloccanti                                       */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       * 04 : (libero)                                         */
/*       * 05 : (libero)                                         */
/*       * 06 : (libero)                                         */
/*       * 07 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_bl1                   varchar (01)                not null,
  flg_bl2                   varchar (01)                not null,
  flg_bl3                   varchar (01)                not null,
  flg_bl4                   varchar (01)                not null,
  flg_bl5                   varchar (01)                not null,
  flg_bl6                   varchar (01)                not null,
  flg_bl7                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags non bloccanti                                   */
/*       *                                                       */
/*       * 01 : (libero)                                         */
/*       * 02 : (libero)                                         */
/*       * 03 : (libero)                                         */
/*       *-------------------------------------------------------*/
  flg_nb1                   varchar (01)                not null,
  flg_nb2                   varchar (01)                not null,
  flg_nb3                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Flags di pulizia effettuabile                         */
/*       *-------------------------------------------------------*/
  flg_pul                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Identificativi                                        */
/*       *-------------------------------------------------------*/
  ide_dat                   date                        not null,
  ide_ute                   varchar (08)                not null,
  ide_fas                   varchar (06)                not null

