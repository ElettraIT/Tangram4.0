#!/usr/bin/perl -w
#==========================================*
# Modulo Perl per test autocompletamento   *
#                                          *
# SU MISURA PER SIRI                       *
#                                          *
# sirioff9.pl                              *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 22/06/17   *
#   Ultima revisione:   wip del 02/09/17   *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
# MEMO : Adeguare sempre $pass per SIRI !  *
#                                          *
#==========================================*
#==========================================*
# Moduli Perl utilizzati                   *
#------------------------------------------*
use CGI                                   ;
use DBI                                   ;
use JSON                                  ;
use strict                                ;
use warnings                              ;

#==========================================*
# Inizio istanza CGI                       *
#------------------------------------------*
my $cgi = CGI->new                        ;

#==========================================*
# Variabili CGI                            *
#------------------------------------------*
my $inp = $cgi->param('term')              ;

#==========================================*
# Variabili di connessione                 *
#------------------------------------------*
my $host = 'localhost'                    ;
my $user = 'root'                         ;
my $pass = 'loppio2'                      ;
my $dtbs = 'ele'                          ;
my $dtab = 'zub'                          ;

#==========================================*
# Connessione Database                     *
#------------------------------------------*
my $connect = DBI->connect("DBI:"
                     . "mysql:database="
                     . "$dtbs;host=$host",
                       $user,
                       $pass,
                      {RaiseError => 1,
                       PrintError => 0})
  or die $DBI::errstr;

#==========================================*
# Preparazione query                       *
#------------------------------------------*
my $query = $connect->prepare(qq{SELECT `des_ubi` AS `value`,`des_ubi` AS `label`,`cod_ubi` AS `id`,`cod_ubi` AS `prov` FROM $dtab where `des_cfl` like ?;});

#==========================================*
# Esecuzione query                         *
#------------------------------------------*
$query->execute('%'.$inp.'%')              ;

#==========================================*
# Risultato query                          *
#------------------------------------------*

my @query_output;
while (my $row = $query->fetchrow_hashref)
{
    push @query_output, $row              ;
}

#==========================================*
# Disconnessione Database                  *
#------------------------------------------*
$connect->disconnect();

#==========================================*
# Risultato JSON                           *
#------------------------------------------*
print $cgi->header(-type => "application/json", -charset => "utf-8");
print JSON::to_json(\@query_output);
