/* File: offerte.sql - Release 12/09/2017 */
CREATE TABLE offerte (
   PRIMARY KEY (protocollo,numero_riga),
   protocollo      bigint  (11) not null,
   numero_riga     integer (07) not null,
   data_offerta    date         not null,
   agente          varchar (10) not null,
   fornitore_ric   varchar (40),
   pn_fornit_ric   varchar (40),
   fornitore_prp   varchar (40),
   pn_fornit_prp   varchar (40),
   tipologia       varchar (40),
   cliente         varchar (40),
   pn_cliente      varchar (40),
   prezzo          decimal (08,04),
   legame          varchar (20),
   quantita_annua  bigint  (13),
   quantita_mo     bigint  (13),
   lead_time       integer (07),
   esito           varchar (40),
   target          decimal (08,04),
   note            text,
   
/* ___ DA IMPLEMENTARE - USO INTERNO */
/* cod_cli         integer (07), */
/* cod_age         integer (07), */
/* cod_fnt         integer (07), */
/* alf_pro         varchar (20), */
/* ute_mod         varchar (10), */
/* dat_mod         datetime    , */
   
   
   utente          varchar (10),
   nome_file       varchar (20),
   caricamento     datetime
);

