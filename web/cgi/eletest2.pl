#!/usr/bin/perl -w
#==========================================*
# Modulo Perl per la funzione Autocomplete *
#                                          *
# Nome del modulo: eletest3.pl             *
#                                          *
# N.B.: Utilizzato da 'cgi/eletest2'       *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 19/04/2021 *
#   Ultima revisione:   wip del 30/05/2023 *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
# In input       : - variabile 'term'      *
#                                          *
# (Configurazione: - 'dtb.ini')            *
#                                          *
# ___ PROVA NON FUNZIONANTE ___            *
#                                          *
#==========================================*

#==========================================*
# Moduli Perl utilizzati                   *
#------------------------------------------*
use CGI                                   ;
use DBI                                   ;
use DBD::mysql                            ;
# use JSON                                  ;

#==========================================*
# Parametri di connessione al database     *
#------------------------------------------*
my $cfg_dtb = "/abd/asc/cfg/dtb.ini"      ;
#
my %cnf_dtb = do $cfg_dtb                 ;
my $dtb_hst = $cnf_dtb{dtb_hst}           ;
my $dtb_usr = $cnf_dtb{dtb_usr}           ;
my $dtb_pwd = $cnf_dtb{dtb_pwd}           ;
#
my $dtb_azi = "ele"                       ;
my $dtb_prt = "3306"                      ;
my $dtb_tbl = "dcp"                       ;

#==========================================*
# Nuova istanza CGI                        *
#------------------------------------------*
my $cgi = CGI->new()                      ;

#==========================================*
# Valori in input                          *
#------------------------------------------*
my $alf_pro = $cgi->param('term')         ;

#==========================================*
# Connessione al database                  *
#------------------------------------------*
$dsn = "dbi:mysql:$dtb_azi:$dtb_hst:$dtb_prt";

#==========================================*
# Apertura connessione                     *
#------------------------------------------*
$connect = DBI->connect($dsn, $dtb_usr, $dtb_pwd) || die print "Can't connect - error...";

#==========================================*
# Preparazione query                       *
#------------------------------------------*
$query = $connect->prepare(qq{select des_pro, alf_pro FROM $dtb_tbl WHERE alf_pro like ? ORDER BY alf_pro ASC;});

#==========================================*
# Header output                            *
#------------------------------------------*
print $cgi->header(-type => "application/json", -charset => "utf-8");

#==========================================*
# Esecuzione query                         *
#------------------------------------------*
$query->execute($alf_pro.'%')             ;




# LOOP THROUGH RESULTS
while ( my $row = $query->fetchrow_hashref ){
    push @query_output, $row;
}
# CLOSE THE DATABASE CONNECTION
$connect->disconnect();

# JSON OUTPUT
print JSON::to_json(\@query_output);

print $alf_pro . "\n\n";
print @query_output;


#==========================================*
# Chiusura connessione                     *
#------------------------------------------*
$connect->disconnect()                    ;

#==========================================*
# SUB-ROUTINES                             *
#==========================================*

