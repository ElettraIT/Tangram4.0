#!/usr/bin/perl -w
#==========================================*
# Modulo Perl per Login gestione tablet    *
#                                          *
# SU MISURA PER ELETTRA ___ PROVE ___      *
#                                          *
# ele_log.pl                               *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 02/07/24   *
#   Ultima revisione:   wip del 19/07/24   *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
#==========================================*

#==========================================*
# Moduli Perl utilizzati                   *
#------------------------------------------*
use strict                                ;
use CGI                                   ;
use CGI::Session                          ;
use DBI()                                 ;
use Digest::MD5 qw(md5_hex)               ;

#==========================================*
# Inizio istanza CGI                       *
#------------------------------------------*
my $cgi = new CGI()                       ;

#==========================================*
# Indirizzi di  base                       *
#------------------------------------------*
my $wdir = $cgi->url(-base=>1)            ;
my $jquy = "$wdir/jsc/jquery.js"          ;
my $jqui = "$wdir/jsc/jquery-ui.js"       ;
my $jscs = "$wdir/jsc_ele/ele_log.js"     ;
my $csss = "$wdir/css_ele/ele_login.css"  ;
my $cfsd = "/abd/asc/tmp/sessioni_cgi"    ;
my $cfgi = "/abd/asc/tmp/ele/cfg.ini"    ;

#==========================================*
# Variabili CGI                            *
#------------------------------------------*
my $cod_ute = $cgi->param("cod_ute")      ;
my $pwd_ute = $cgi->param("pwd_ute")      ;

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
# Visualizzazione iniziale                 *
#------------------------------------------*
if (!$cod_ute || !$pwd_ute )
{
    #--------------------------------------*
    # Intestazione per HTML                *
    #--------------------------------------*
    print "Content-type: text/html\n\n"   ;
    #--------------------------------------*
    # Visualizzazione form                 *
    #--------------------------------------*
    DisplayForm()                         ;
    #--------------------------------------*
    # Uscita                               *
    #--------------------------------------*
    exit()                                ;
}
 else
{
    #--------------------------------------*
    # Test su Utente e Password            *
    #--------------------------------------*
    my ($rec_ute,
        $des_ute,
        $tip_ute) = TestData($cod_ute,
                             $pwd_ute)    ;
    #--------------------------------------*
    # Se trovato un solo record            *
    #--------------------------------------*
    if ($rec_ute == 1)
    {
        #----------------------------------*
        # Inizializzazione sessione        *
        #----------------------------------*
        my $s = new CGI::Session("driver:File",
                                      undef,
                      {Directory=>$cfsd})  ;    
        #----------------------------------*
        # ID di sessione                   *
        #----------------------------------*
        my $sid = $s->id()                ;
        #----------------------------------*
        # Durata sessione: 10 h.           *
        #----------------------------------*
        $s->expire('+10h')                ;
        #----------------------------------*
        # Memorizzazione utente            *
        #----------------------------------*
        $s->param("cod_ute", $cgi->param("cod_ute"));
        #----------------------------------*
        # Memorizzazione descr. utente     *
        #----------------------------------*
        $s->param("des_ute", $des_ute)    ;
        #----------------------------------*
        # Memorizzazione tipo utente       *
        #----------------------------------*
        $s->param("tip_ute", $tip_ute)    ;
        #----------------------------------*
        # Memorizzazione IP Client         *
        #----------------------------------*
        $s->param("ind_ipx", $ENV{REMOTE_ADDR});
        #----------------------------------*
        # Memorizzazione modulo di default *
        # per la visualizzazione           *
        #----------------------------------*
        $s->param("cal_ler", "vis")       ;
        #----------------------------------*
        # A menu' iniziale                 *
        #----------------------------------*
        my $page = "ele_ind.pl"
                 . "?ids="
                 . $sid                   ;
        print $cgi->redirect($page)       ;
        #----------------------------------*
        # Intestazione per HTML            *
        #----------------------------------*
        print "Content-type: text/html\n\n";
        #----------------------------------*
        # Uscita                           *
        #----------------------------------*
        exit()                            ;
    }
     else
    {
        #----------------------------------*
        # Ricarica pagina                  *
        #----------------------------------*
        

    #======================================*
    # DEBUG                                *
    #--------------------------------------*
    print "Content-type: text/html\n\n"   ;
    print "rec_ute = " . $rec_ute . "\n"    ;

    print "des_ute = " . $des_ute . "\n"    ;
    print "tip_ute = " . $tip_ute . "\n"    ;
    exit()                                  ;

        
        
        my $page = "ele_log.pl"           ;
        print $cgi->redirect($page)       ;
        #----------------------------------*
        # Uscita                           *
        #----------------------------------*
        exit()                            ;
    }
}

#==========================================*
# Disconnessione Database                  *
#------------------------------------------*
$dtb_con->disconnect                      ;

#==========================================*
# FINE SCRIPT                              *
#==========================================*

#==========================================*
# Subroutines                              *
#==========================================*

#==========================================*
# Test valori in entrata                   *
#------------------------------------------*
sub TestData
{
    #--------------------------------------*
    # Valori in entrata                    *
    #--------------------------------------*
    my $usr = shift                       ;
    my $pwd = shift                       ;
    #--------------------------------------*
    # Crypt della password                 *
    #--------------------------------------*
    $pwd = md5_hex($pwd)                  ;
    #--------------------------------------*
    # Preparazione Select                  *
    #--------------------------------------*
    my $statement = qq{SELECT des_ute, tip_ute FROM ute WHERE cod_ute=? and pwd_ute=?};
    my $sth_dtb = $dtb_con->prepare($statement)
     or die $dtb_con->errstr              ;
    #--------------------------------------*
    # Esecuzione Select                    *
    #--------------------------------------*
    $sth_dtb->execute($usr, $pwd)
     or die $sth_dtb->errstr              ;
    #--------------------------------------*
    # Risultato righe lette                *
    #--------------------------------------*
    my $rec_ute = $sth_dtb->rows          ;
    #--------------------------------------*
    # Raccolta dati letti                  *
    #--------------------------------------*
    my ($des_ute, $tip_ute) = $sth_dtb->fetchrow_array;    
    #--------------------------------------*
    # Dati letti in uscita                 *
    #--------------------------------------*
    return $rec_ute, $des_ute, $tip_ute   ;
}

#==========================================*
# Visualizza pagina Web                    *
#------------------------------------------*
sub DisplayForm
{
print <<"HTML";
<html>
<!-- +=================================+ -->
<!-- | wip - Tutti i diritti riservati | -->
<!-- | ------------------------------- | -->
<!-- | Aggiornamento      : 19/07/2024 | -->
<!-- +=================================+ -->
<head>
<!-- *=================================* -->
<!-- * Titolo                          * -->
<!-- *=================================* -->
<title>{ ELETTRA }</title>
<!-- *=================================* -->
<!-- * CSS                             * -->
<!-- *=================================* -->
<link rel='stylesheet' type='text/css' href='$csss'>
<!-- *=================================* -->
<!-- * Javascript                      * -->
<!-- *=================================* -->
<script src='$jquy'    TYPE='text/javascript'></script>
<script src='$jscs'    TYPE='text/javascript'></script>
</head>
<!-- *=================================* -->
<!-- * Body                            * -->
<!-- *=================================* -->
<body>
<!-- *=================================* -->
<!-- * Logo                            * -->
<!-- *=================================* -->
<hr size=3>
<center>
<a href='$wdir'>
<img src='$wdir/icons/logo-elettra.png'>
</a>
</center>
<hr size=3>
<!-- *=================================* -->
<!-- * Login                           * -->
<!-- *=================================* -->
<div class="login">
<h1>TABLET login</h1>
<!-- *---------------------------------* -->
<!-- * Form                            * -->
<!-- *---------------------------------* -->
<form method='post' id='log_inn' action=''>
<!-- *---------------------------------* -->
<!-- * Utente                          * -->
<!-- *---------------------------------* -->
<input  type="text" 
          id="cod_ute" 
        name="cod_ute" 
 placeholder="Utente" 
    required="required" />
<!-- *---------------------------------* -->
<!-- * Password                        * -->
<!-- *---------------------------------* -->
<input  type="password" 
          id="pwd_ute" 
        name="pwd_ute" 
 placeholder="Password" 
    required="required" 
autocomplete="off" />
<!-- *---------------------------------* -->
<!-- * Conferma                        * -->
<!-- *---------------------------------* -->
<button type="button"
          id="conferma"
        name="conferma"
       class="btn btn-primary btn-block btn-large">ACCEDI</button>
</form>
</div>
<!-- *=================================* -->
<!-- * Box Info                        * -->
<!-- *=================================* -->
<div>
<style type='text/css'>p1 {font-size : 24px;}</style>
<center><p1><span id='info' name 'info'></span></p1></center>
</div>
<!-- *=================================* -->
<!-- * Copyright                       * -->
<!-- *=================================* -->
<center>
    <div id="copy" name="copy">
        <address>
            <font size=3>&copy wip - TANGRAM
        </address>
    </div>
</center>
<!-- *=================================* -->
<!-- * Chiusura Body e Html            * -->
<!-- *=================================* -->
</body>
</html>
HTML
}

