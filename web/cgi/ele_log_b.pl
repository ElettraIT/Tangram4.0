#!/usr/bin/perl -w
#==========================================*
# Modulo Perl per lettura Offerta          *
#                                          *
# ele_log_b.pl                             *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 19/07/2024 *
#   Ultima revisione:   NdK del 19/07/2024 *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
# Richiamato da 'ele_log.js'               *
#                                          *
#------------------------------------------*

#==========================================*
# Moduli Perl utilizzati                   *
#------------------------------------------*
use CGI                                   ;
use DBI                                   ;
use strict                                ;
use Digest::MD5 qw(md5_hex)               ;

#==========================================*
# Variabili CGI                            *
#------------------------------------------*
my $cgi = CGI->new                        ;
my $cod_ute = $cgi->param("cod_ute")      ;
my $pwd_ute = $cgi->param("pwd_ute")      ;




    #======================================*
    # DEBUG                                *
    #--------------------------------------*
#    print "Content-type: text/html\n\n"   ;
#    print "cod_ute = " . $cod_ute . "\n"    ;

#    print "pwd_ute = " . $pwd_ute . "\n"    ;
#    exit()                                  ;



#==========================================*
# Crypt della password                     *
#------------------------------------------*
$pwd_ute = md5_hex($pwd_ute)              ;

#==========================================*
# Parametri di connessione al database     *
#------------------------------------------*
my $cfg_dtb = "/abd/asc/cfg/dtb_log.ini"  ;
#
my %cnf_dtb = do $cfg_dtb                 ;
my $dtb_hst = $cnf_dtb{dtb_hst}           ;
my $dtb_usr = $cnf_dtb{dtb_usr}           ;
my $dtb_pwd = $cnf_dtb{dtb_pwd}           ;
#
my $dtb_dtb = "xpg"                       ;
my $dtb_tbt = "ute"                       ;

#==========================================*
# Connessione database                     *
#------------------------------------------*
my $dtb_con = DBI->connect("DBI:"
                         . "mysql:database="
                         . "$dtb_dtb;"
                         . "host=$dtb_hst",
                                  $dtb_usr,
                                  $dtb_pwd,
                          {RaiseError => 1,
                           PrintError => 0});

#==========================================*
# Preparazione Select                      *
#------------------------------------------*
my $sel_dtb = qq{SELECT cod_ute, pwd_ute, des_ute FROM ute WHERE cod_ute=? and pwd_ute=?};
my $sth_dtb = $dtb_con->prepare($sel_dtb)
  or die $dtb_con->errstr                 ;

#==========================================*
# Esecuzione Select                        *
#------------------------------------------*
$sth_dtb->execute($cod_ute, $pwd_ute)
  or die $sth_dtb->errstr;
my ($cod, $pwd, $des) = $sth_dtb->fetchrow_array;

#==========================================*
# Disconnessione                           *
#------------------------------------------*
$dtb_con->disconnect()                    ;

#==========================================*
# Emissione risultato                      *
#------------------------------------------*
my $json = ($cod) ? 
  qq{{"success" : "Login corretto", "userid" : "$cod", "password" : "$pwd", "descr" : "$des"}} : 
  qq{{"error" : "Utente o password errati"}};

#==========================================*
# Formato JSON                             *
#------------------------------------------*
print $cgi->header(-type => "application/json", -charset => "utf-8");
print $json                               ;
