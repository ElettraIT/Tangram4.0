/*    *==========================================================*/
/*    * Tabella [zmu] Tipi ubicazione di magazzino         {mag} */
/*    *                                                          */
/*    * Aggiornamento 20.01.2005                                 */
/*    *----------------------------------------------------------*/
/*       *=======================================================*/
/*       * Definizione delle chiavi                              */
/*       *-------------------------------------------------------*/
  PRIMARY KEY                                          (cod_dpz,
                                                        tip_ubi),
/*       *=======================================================*/
/*       * Codice dipendenza                                     */
/*       *-------------------------------------------------------*/
  cod_dpz                   integer (07)                not null,
/*       *-------------------------------------------------------*/
/*       * Tipo ubicazione                                       */
/*       *-------------------------------------------------------*/
  tip_ubi                   varchar (03)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo ubicazione                           */
/*       *-------------------------------------------------------*/
  des_ubi                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione tipo ubicazione in uppercase              */
/*       *-------------------------------------------------------*/
  des_key                   varchar (40)                not null,
/*       *-------------------------------------------------------*/
/*       * Commento al tipo ubicazione                           */
/*       *-------------------------------------------------------*/
  com_ubi                   varchar (80)                not null,
/*       *-------------------------------------------------------*/
/*       * Numero parametri di ubicazione: 1..6                  */
/*       *-------------------------------------------------------*/
  num_pdu                   tinyint (01)                not null,
/*       *-------------------------------------------------------*/
/*       * Descrizione parametri di ubicazione: 1..6             */
/*       *-------------------------------------------------------*/
  des_pu1                   varchar (10)                not null,
  des_pu2                   varchar (10)                not null,
  des_pu3                   varchar (10)                not null,
  des_pu4                   varchar (10)                not null,
  des_pu5                   varchar (10)                not null,
  des_pu6                   varchar (10)                not null,
  
/*       *-------------------------------------------------------*/
/*       * Lunghezza parametri di ubicazione: 1..6               */
/*       *-------------------------------------------------------*/
  chr_pu1                   tinyint (01)                not null,
  chr_pu2                   tinyint (01)                not null,
  chr_pu3                   tinyint (01)                not null,
  chr_pu4                   tinyint (01)                not null,
  chr_pu5                   tinyint (01)                not null,
  chr_pu6                   tinyint (01)                not null  