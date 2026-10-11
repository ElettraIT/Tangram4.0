#!/usr/bin/perl -w
#==========================================*
# Modulo Perl per menu' principale Elettra *
#                                          *
# SU MISURA PER ELETTRA                    *
#                                          *
# ele_ind.pl                               *
#                                          *
# ___ DA IMPLEMENTARE ___                  *
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
use strict                                ;
use CGI                                   ;
use CGI::Session                          ;
use DBI()                                 ;

#==========================================*
# Inizio istanza CGI                       *
#------------------------------------------*
my $cgi = new CGI()                       ;

#==========================================*
# Indirizzi di  base                      *
#------------------------------------------*
my $wdir = $cgi->url(-base=>1)            ;
my $sdir = "$wdir/cgi-bin"                ;
my $jquy = "$wdir/jsc/jquery.js"          ;
my $jqui = "$wdir/jsc/jquery-ui.js"       ;
my $jscs = "$wdir/jsc_ele/ele_ind.js"     ;
my $csss = "$wdir/css_ele/ele.css"        ;

#==========================================*
# Indirizzi moduli                         *
#------------------------------------------*
my $mlog = "$sdir/ele_log.pl"             ;
my $mcar = "$sdir/elebfoB1"               ;
my $mpro = "$sdir/elepro01"               ;


my $mind = "$sdir/sirioff_ind.pl"         ;
my $mvis = "$sdir/sirioff_vis.pl"         ;
my $msts = "$sdir/sirioff_sts.pl"         ;
my $mdel = "$sdir/sirioff_del.pl"         ;

#==========================================*
# Variabili CGI                            *
#------------------------------------------*
my $ids = $cgi->param("ids")              ;
my $log = $cgi->param("log")              ;

#==========================================*
# Test su variabile di sessione            *
#------------------------------------------*
my $s = new CGI::Session("driver:File",
                                  $ids,
  {Directory=>'/abd/asc/tmp/sessioni_cgi'});    
#------------------------------------------*
# Elementi di sessione                     *
#------------------------------------------*
my $sid = $s->id()                        ;
my $uid = $s->param("ind_ipx")            ;
my $sut = $s->param("cod_ute")            ;
my $dut = $s->param("des_ute")            ;
my $tut = $s->param("tip_ute")            ;

#------------------------------------------*
# Default modulo in esecuzione             *
#------------------------------------------*
$s->param("cal_ler", "mnu")               ;

#------------------------------------------*
# Controllo con IP client                  *
#------------------------------------------*
my $ipc = $ENV{REMOTE_ADDR}               ;

#==========================================*
# Test su IP client                        *
#------------------------------------------*
if ($ipc ne $uid)
{
    #--------------------------------------*
    # Chiusura e cancellazione sessione    *
    #--------------------------------------*
    $s->delete()                          ;
    #--------------------------------------*
    # A pagina iniziale                    *
    #--------------------------------------*
    print $cgi->redirect($mlog)           ;
    #--------------------------------------*
    # Uscita                               *
    #--------------------------------------*
    exit()                                ;
}

#==========================================*
# Test se logout                           *
#------------------------------------------*
if ($log eq "out")
{
    #--------------------------------------*
    # Chiusura e cancellazione sessione    *
    #--------------------------------------*
    $s->delete()                          ;
    #--------------------------------------*
    # A pagina iniziale                    *
    #--------------------------------------*
    print $cgi->redirect($mlog)           ;
    #--------------------------------------*
    # Uscita                               *
    #--------------------------------------*
    exit()                                ;
}

#==========================================*
# Visualizzazione iniziale                 *
#------------------------------------------*
if ($ids eq $sid)
{
    #--------------------------------------*
    # Intestazione per HTML                *
    #--------------------------------------*
    print "Content-type: text/html\n\n"   ;
    #--------------------------------------*
    # Visualizzazione form                 *
    # ___ DIVIDERE SE SUPERVISORE ___      *
    #--------------------------------------*
    DisplayForm()                         ;
    exit()                                ;
}
 else
{
    #--------------------------------------*
    # A pagina iniziale                    *
    #--------------------------------------*
    print $cgi->redirect($mlog)           ;
    #--------------------------------------*
    # Intestazione per HTML                *
    #--------------------------------------*
    print "Content-type: text/html\n\n"   ;
}

#==========================================*
# FINE SCRIPT                              *
#==========================================*

#==========================================*
# Subroutines                              *
#==========================================*

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
<!-- +---------------------------------+ -->
<!-- | Testata modulo                  | -->
<!-- +---------------------------------+ -->
<head>
<!-- *=================================* -->
<!-- * Titolo                          * -->
<!-- *=================================* -->
<title>{ ELETTRA }</title>
<!-- *=================================* -->
<!-- * CSS                             * -->
<!-- *=================================* -->
<link rel='stylesheet' href='$csss' type='text/css'>
<link rel='stylesheet' href='//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css'>
<!-- *=================================* -->
<!-- * Javascript                      * -->
<!-- *=================================* -->
<script src="https://code.jquery.com/jquery-1.12.4.js"></script>
<script src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"></script>
<script src='$jscs'    TYPE='text/javascript'></script>
</head>
<!-- *=================================* -->
<!-- * Body                            * -->
<!-- *=================================* -->
<body>
<!-- *=================================* -->
<!-- * Tabella parte superiore         * -->
<!-- *=================================* -->
<table class='bordotab' border='3' width=100%>
<tr align='center'>
<!--    *------------------------------* -->
<!--    * Box utente                   * -->
<!--    *------------------------------* -->
<td width=10%><p1>[$sut]</p1></td>
<!--    *------------------------------* -->
<!--    * Box Logo                     * -->
<!--    *------------------------------* -->
<td width=80%>
<center>
<a href='$mind?ids=$sid'>
<img src='$wdir/icons/logo-elettra.png'>
</a>
</center>
</td>
<!--    *------------------------------* -->
<!--    * Box area                     * -->
<!--    *------------------------------* -->
<td width=10%><p1>TABLET</p1></td>
</tr>
</table>
<!-- *=================================* -->
<!-- * Tabella parte superiore - fine  * -->
<!-- *=================================* -->
<br>
<br>
<center>
<!-- *=================================* -->
<!-- * Form                            * -->
<!-- *=================================* -->
<form method='post' 
          id='menu'
      action=''>
<!-- *=================================* -->
<!-- * Menu'                           * -->
<!-- *=================================* -->
<table class='bordotab' width=50%>

<!--    *==============================* -->
<!--    * DOCUMENTI DI CARICO          * -->
<!--    *------------------------------* -->
        <tr data-url='$mcar?ids=$sid'
               align='center'>

            <td class='link' 
                width='150' 
                height='20%'>
                <a href='$mcar?ids=$sid'>
                    <img border='0' 
                            src='$wdir/icons/camion.png'
                            alt='CARICO' 
                          width='40%'>
                </a>
            </td>

            <td class='link' 
                width='300'>
                <a href='/cgi-bin/elebfoB1'>DOCUMENTI DI CARICO - bfo</a>
            </td>
        </tr>

<!--    *==============================* -->
<!--    * SCHEDA PRODOTTO              * -->
<!--    *------------------------------* -->
        <tr data-url='/cgi-bin/elepro01' 
               align='center'>
            <td class='link' 
                width='150' 
                height='20%'>
                <a href='/cgi-bin/elepro01'>
                    <img border='0' 
                            src='$wdir/icons/prodotto2.jpg'
                            alt='PRODOTTO' 
                          width='30%'>
                </a>
            </td>

            <td class='link' 
                width='300'>
                <a href='/cgi-bin/elepro01'>SCHEDA PRODOTTO - mag301</a>
            </td>
        </tr>

<!--    *==============================* -->
<!--    * SPOSTAMENTO UBICAZIONI       * -->
<!--    *------------------------------* -->
        <tr data-url='/cgi-bin/eleubi01' 
               align='center'>

            <td class='link' 
                width='150' 
                height='20%'>
                <a href='/cgi-bin/eleubi01'>
                    <img border='0' 
                            src='$wdir/icons/ubicazioni.jpg'
                            alt='UBICAZIONI' 
                          width='30%'>
                </a>
            </td>

            <td class='link' 
                width='300'>
                <a href='/cgi-bin/eleubi01'>UBICAZIONI - SPOSTAMENTO PRODOTTI</a>
            </td>
        </tr>

<!--    *==============================* -->
<!--    * PREPARAZIONE SPEDIZIONI      * -->
<!--    *------------------------------* -->
        <tr data-url='/cgi-bin/eleodsB1' 
               align='center'>

            <td class='link' 
                width='150' 
                height='20%'>
                <a href='/cgi-bin/eleodsB1'>
                    <img border='0' 
                            src='$wdir/icons/box.png'
                            alt='SPEDIZIONI' 
                          width='40%'>
                </a>
            </td>

            <td class='link' 
                width='300'>
                <a href='/cgi-bin/eleodsB1'>PREPARAZIONE SPEDIZIONI - ods</a>
            </td>
        </tr>

<!--    *==============================* -->
<!--    * SITUAZIONE UBICAZIONI        * -->
<!--    *------------------------------* -->
        <tr data-url='/cgi-bin/eleubi02' 
               align='center'>

            <td class='link' 
                width='150' 
                height='20%'>
                <a href='/cgi-bin/eleubi02'>
                    <img border='0' 
                            src='$wdir/icons/ubicazioni3.png'
                            alt='UBICAZIONI' 
                          width='40%'>
                </a>
            </td>

            <td class='link' 
                width='300'>
                <a href='/cgi-bin/eleubi02'>UBICAZIONI - Situazione</a>
            </td>
        </tr>

<!--    *==============================* -->
<!--    * CONFERMA USCITA MERCE        * -->
<!--    *------------------------------* -->
        <tr data-url='/cgi-bin/elebol01' 
               align='center'>

            <td class='link' 
                width='150' 
                height='20%'>
                <a href='/cgi-bin/elebol01'>
                    <img border='0' 
                            src='$wdir/icons/bol_cnf.png'
                            alt='MERCE' 
                          width='40%'>
                </a>
            </td>

            <td class='link' 
                width='300'>
                <a href='/cgi-bin/elebol01'>MERCE - Conferma uscita</a>
            </td>
        </tr>

<!--    *==============================* -->
<!--    * CHIUSURA TABELLA             * -->
<!--    *------------------------------* -->
</table>
<br>
<br>

<!-- *=================================* -->
<!-- * Bottone 'logout'                * -->
<!-- *=================================* -->
<input type='image'  
         id='logout' 
       name='logout' 
        src='$wdir/icons/uscita.png' 
     border='0' 
        alt='submit'  
      width='60' />
<!-- *=================================* -->
<!-- * Form - fine                     * -->
<!-- *=================================* -->
</form>
</center>
<br>
<!-- *=================================* -->
<!-- * Box per Dialog uscita           * -->
<!-- *=================================* -->
<div id='dialog'></div>
<br>
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

