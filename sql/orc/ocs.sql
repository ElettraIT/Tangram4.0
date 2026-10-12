/*    *==========================================================*/
/*    * Tabella [ocs] Ordini clienti                       {orc} */
/*    *                                                          */
/*    * Estensione a testate : castelletto spese                 */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (num_prt,
                                                        prg_cst),
/*       *=======================================================*/
/*       * Numero protocollo : s.aa.dd.nnnnnn                    */
/*       *-------------------------------------------------------*/
  num_prt                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Progressivo castelletto                               */
/*       *-------------------------------------------------------*/
  prg_cst                   integer (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Si/No addebito spese                                  */
/*       *                                                       */
/*       * - 0 : No addebito                                     */
/*       * - 1 : Si addebito                                     */
/*       *-------------------------------------------------------*/
  spe_snx                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Modalita di addebito spese                            */
/*       *                                                       */
/*       * - 1 : Addebito a percentuale fissa                    */
/*       * - 2 : Addebito a percentuale variabile                */
/*       * - 3 : Addebito ad importo fisso                       */
/*       * - 4 : Addebito ad importo variabile                   */
/*       *-------------------------------------------------------*/
  spe_mad                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * % di addebito della spesa                             */
/*       *-------------------------------------------------------*/
  spe_per                   decimal (05,02)             not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di imponibile per la % della spesa               */
/*       *                                                       */
/*       * - 00 : Non significativo                              */
/*       * - 01 : Totale lordo                                   */
/*       * - 02 : Totale netto                                   */
/*       * - 03 : Su una combinazione delle voci seguenti        */
/*       * - 11 : Totale merci                                   */
/*       * - 12 : Totale servizi                                 */
/*       * - 13 : Totale imballi                                 */
/*       * - 14 : Totale libero 4                                */
/*       * - 15 : Totale libero 5                                */
/*       * - 16 : Totale libero 6                                */
/*       * - 17 : Totale libero 7                                */
/*       * - 18 : Totale libero 8                                */
/*       * - 19 : Totale extra                                   */
/*       *-------------------------------------------------------*/
  spe_ibl                   tinyint (02)                not null,
/*       *-------------------------------------------------------*/
/*       * Totalizzatori da considerarsi per la determinazione   */
/*       * imponibile per la % di spesa, in valuta               */
/*       *                                                       */
/*       * Solo se imponibile per la % pari a 03                 */
/*       *                                                       */
/*       * Per ogni totalizzatore deve essere specificato un     */
/*       * valore di un carattere, dove :                        */
/*       *                                                       */
/*       * - N : Non concorre a formare imponibile               */
/*       * - S : Concorre a formare imponibile                   */
/*       *                                                       */
/*       * Il significato dei totalizzatori e' di tipo           */
/*       * posizionale, con la seguente codifica :               */
/*       *                                                       */
/*       * 1 : Totale merci                                      */
/*       * 2 : Totale servizi                                    */
/*       * 3 : Totale imballi                                    */
/*       * 4 : Totale libero 4                                   */
/*       * 5 : Totale libero 5                                   */
/*       * 6 : Totale libero 6                                   */
/*       * 7 : Totale libero 7                                   */
/*       * 8 : Totale libero 8                                   */
/*       * 9 : Totale extra                                      */
/*       *-------------------------------------------------------*/
  ibt_sp1                   varchar (01)                not null,
  ibt_sp2                   varchar (01)                not null,
  ibt_sp3                   varchar (01)                not null,
  ibt_sp4                   varchar (01)                not null,
  ibt_sp5                   varchar (01)                not null,
  ibt_sp6                   varchar (01)                not null,
  ibt_sp7                   varchar (01)                not null,
  ibt_sp8                   varchar (01)                not null,
  ibt_sp9                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Importo addebito della spesa, in valuta               */
/*       *-------------------------------------------------------*/
  spe_imp                   bigint  (11)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice iva per la spesa                               */
/*       *                                                       */
/*       * N.B. : Se a zero significa che la spesa non e'        */
/*       *        soggetta a particolare codice iva, ma che      */
/*       *        l'importo spesa va' sventagliato in propor-    */
/*       *        zione ai codici iva gia' presenti nel          */
/*       *        documento                                      */
/*       *-------------------------------------------------------*/
  spe_civ                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice contropartita per la spesa                     */
/*       *                                                       */
/*       * N.B. : Se a zero significa che la spesa non si        */
/*       *        riferisce ad una particolare contropartita,    */
/*       *        ma che l'importo spesa va' sventagliato in     */
/*       *        proporzione alle contropartite gia' presenti   */
/*       *        nel documento                                  */
/*       *-------------------------------------------------------*/
  spe_ccp                   integer (07)                not null
