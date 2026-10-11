#!/usr/bin/perl -w
#==========================================*
# Modulo Perl per la funzione Autocomplete *
#                                          *
# Nome del modulo: eleproA0.pl             *
#                                          *
# N.B.: Utilizzato da 'cgi/elepro01'       *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 19/04/2021 *
#   Ultima revisione:   wip del 25/10/2024 *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
# In input       : - variabile 'term'      *
#                                          *
# (Configurazione: - 'dtb.ini')            *
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
# Esecuzione query                         *
#------------------------------------------*
$query->execute('%' . $alf_pro . '%')      ;

#==========================================*
# Header output                            *
#------------------------------------------*
print $cgi->header(-type => "application/json", -charset => "utf-8");

#==========================================*
# Inizializzazione output in stile JSON    *
#------------------------------------------*
print "{"                                 ;

#==========================================*
# Numero delle righe estratte              *
#------------------------------------------*
my $num_row = $query->rows                ;

#==========================================*
# Normalizzazione contatore di comodo      *
#------------------------------------------*
my $ctr_001                               ;

#==========================================*
# Ciclo di scansione righe estratte        *
#                                          *
# Necessario a sopperire il riordino ca-   *
# suale delle righe effettuato da          *
# 'fetchrow'                               *
#------------------------------------------*
while(my @res_qry = $query->fetchrow_array())
{
    #--------------------------------------*
    # Formattazione riga in stile JSON     *
    #--------------------------------------*
    my $rig_fmt = &jsn_frm ($res_qry[0],
                            $res_qry[1])  ;
    print $rig_fmt                        ;
    #--------------------------------------*
    # Incremento contatore                 *
    #--------------------------------------*
    $ctr_001 = $ctr_001+1                 ;
    #--------------------------------------*
    # Test se ultimo elemento              *
    #--------------------------------------*
    if ($ctr_001 != $num_row)
    {print ","}                           ;
}

#==========================================*
# Chiusura output in stile JSON            *
#------------------------------------------*
print "}"                                 ;

#==========================================*
# Chiusura connessione                     *
#------------------------------------------*
$connect->disconnect()                    ;

#==========================================*
# SUB-ROUTINES                             *
#==========================================*

#==========================================*
# Formattazione in stile JSON              *
#------------------------------------------*
sub jsn_frm ($val1, $val2)
{
    #--------------------------------------*
    # Elementi in input                    *
    #--------------------------------------*
    my $val_in1 = shift                    ;
    my $val_in2 = shift                    ;
    #--------------------------------------*
    # Composizione                         *
    #--------------------------------------*
    my $rig_out = "\""
                . $val_in1
                . "\":\""
                . $val_in2
                . "\""                    ;
    #--------------------------------------*
    # Valore in output                     *
    #--------------------------------------*
    return $rig_out                       ;
}



