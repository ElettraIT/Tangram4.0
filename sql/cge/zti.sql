/*    *==========================================================*/
/*    * Tabella [zti] tabella codici tipi operazione per   {cge} */
/*    *               la comunicazione iva                       */
/*    *                                                          */
/*    * Aggiornamento 09.11.2025                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_top),
/*       *=======================================================*/
/*       * Codice tipo operazione                                */
/*       *-------------------------------------------------------*/
/*       *                                                       */
/*       *         Tabella tipi operazione predefiniti           */
/*       *                                                       */
/*       *   ------------------ FRONTESPIZIO ------------------  */
/*       *                                                       */
/*       * - 01110 Codice Fornitura                              */
/*       * - 01120 Codice Fiscale Dichiarante                    */
/*       * - 01130 Codice Carica                                 */
/*       *                                                       */
/*       * - 02110 Codice Fiscale                                */
/*       * - 02120 Anno Imposta                                  */
/*       * - 02130 Partita IVA                                   */
/*       * - 02170 CF Dichiarante                                */
/*       * - 02180 Codice Carica Dichiarante                     */
/*       *                                                       */
/*       * - 02200 Firma Dichiarazione                           */
/*       * - 02210 CF Intermediario                              */
/*       * - 02220 Impegno Presentazione                         */
/*       * - 02230 Data Impegno                                  */
/*       * - 02240 Firma Intermediario                           */
/*       * - 02260 Identificativo Prod Software                  */
/*       *                                                       */
/*       *   ------------------- MODULI (3) -------------------  */
/*       *                                                       */
/*       * - 05005 Numero Modulo                                 */
/*       * - 05010 Mese                                     VP1  */
/*       * - 05050 Totale Operazioni Attive                 VP2  */
/*       * - 05060 Totale Operazioni Passive                VP3  */
/*       * - 05070 Iva Esigibile                            VP4  */
/*       * - 05080 Iva Detratta                             VP5  */
/*       * - 05090 Iva Dovuta                               VP6  */
/*       * - 05100 Iva Credito                              VP6  */
/*       * - 05110 Debito Precedente                        VP7  */
/*       * - 05120 Credito Periodo Precedente               VP8  */
/*       * - 05130 Credito Anno Precedente                  VP9  */
/*       * - 05140 Versamenti Auto UE                       VP10 */
/*       * - 05150 Crediti imposta                          VP11 */
/*       * - 05160 Interessi Dovuti                         VP12 */
/*       * - 05165 Metodo                                   VP13 */
/*       * - 05170 Acconto                                  VP13 */
/*       * - 05180 Importo Da Versare                       VP14 */
/*       * - 05190 Importo A Credito                        VP14 */
/*       *-------------------------------------------------------*/
  cod_top                   integer (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Codice mnemonico per il tipo operazione               */
/*       *-------------------------------------------------------*/
  cod_mne                   varchar (05)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo operazione                           */
/*       *-------------------------------------------------------*/
  des_top                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di campo associato                               */
/*       *                                                       */
/*       * - 'A' : Anno                                          */
/*       * - 'M' : Mese                                          */
/*       * - 'N' : Numerico intero                               */
/*       * - 'D' : Data                                          */
/*       * - 'T' : Campo testuale                                */
/*       * - 'I' : Importo in valuta base                        */
/*       * - 'B' : Valore binario (0,1)                          */
/*       *-------------------------------------------------------*/
  tip_cmp                   varchar (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo di obbligatorieta' del campo associato           */
/*       *                                                       */
/*       * - 'F' : Campo facoltativo                             */
/*       * - 'O' : Campo obbligatorio                            */
/*       *-------------------------------------------------------*/
  tip_obl                   varchar (01)                not null
