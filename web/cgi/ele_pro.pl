#!/usr/bin/perl -w
#==========================================*
# Modulo Perl per di supporto alla lettura *
# prodotti per Autocomplete                *
#                                          *
# SU MISURA PER ELETTRA                    *
#                                          *
# ele_pro.pl                               *
#                                          *
# ____ IN LAVORAZIONE ____                 *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 02/07/24   *
#   Ultima revisione:   wip del 02/07/24   *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
#==========================================*

#==========================================*
# Moduli Perl utilizzati                   *
#------------------------------------------*
use CGI                                   ;
use DBI                                   ;
use DBD::mysql                            ;
use JSON                                  ;

#==========================================*
# Parametri di connessione al database     *
#------------------------------------------*
my $database = "ele"                      ;
my $table = "dcp"                         ;
my $host = "localhost"                    ;
my $port = "3306"                         ;
my $dtb_tbl = "dcp"                       ;
my $user = "root"                         ;
my $pass = "loppio2"                      ;

#==========================================*
# Inizio istanza CGI                       *
#------------------------------------------*
my $cgi = CGI->new()                      ;

#==========================================*
# Variabili CGI                            *
#------------------------------------------*
my $nazione = $cgi->param('term')         ;

#==========================================*
# Connessione al database                  *
#------------------------------------------*
$dsn = "dbi:mysql:$database:$host:$port"  ;

# open the database connection
$connect = DBI->connect($dsn, $user, $pass) || die print "Can't connect - error...";

# prepare the query
#$query = $connect->prepare(qq{select den_azi, nam_xml FROM xml_for where den_azi like ?;});
$query = $connect->prepare(qq{select des_pro, alf_pro FROM $dtb_tbl WHERE alf_pro like ? ORDER BY alf_pro ASC;});

# execute the query
$query->execute('%'.$nazione.'%');

# obtain the results
my $json = {};
while(my @den_azi = $query->fetchrow_array()) {
   $json->{$den_azi[0]} = $den_azi[1];
}

# close the database connection
$connect->disconnect();

# print the json output to be returned to the HTML page
print $cgi->header(-type => "application/json", -charset => "utf-8");
print JSON::to_json($json);

