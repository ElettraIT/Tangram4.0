#!/usr/bin/perl -w
#==========================================*
# Modulo Perl per caricare immagini        *
#                                          *
# ___ ELETTRA - PROVE ___                  *
#                                          *
# elebfoi0.pl                              *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 13/03/2018 *
#   Ultima revisione:   wip del 30/05/2023 *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
# Mappato per 'wip' funzionante            *
#                                          *
#==========================================*

#==========================================*
# Moduli Perl utilizzati                   *
#------------------------------------------*
use strict                                ;
use CGI                                   ;
use File::Basename                        ;

#==========================================*
# Inizio istanza CGI                       *
#------------------------------------------*
my $cgi = new CGI()                       ;

#==========================================*
# Indirizzi di  base                       *
#------------------------------------------*
my $wdir = $cgi->url(-base=>1)            ;
my $sdir = "$wdir/cgi-bin"                ;
my $jquy = "$wdir/jsc_ele/jquery.js"      ;
my $jscs = "$wdir/jsc_ele/elebfoi0.js"    ;
my $csss = "$wdir/css_ele/ele.css"        ;

#==========================================*
# Variabili CGI                            *
#------------------------------------------*
my $prt_bfo = $cgi->param('prt_bfo')      ;
my $prg_bfo = $cgi->param('prg_bfo')      ;
my $upfile = $cgi->param('file_jpg')      ;

#==========================================*
# Variabili statiche                       *
#------------------------------------------*
my $udir = "/abd/asc/img/"                ;

#==========================================*
# Visualizzazione iniziale                 *
#------------------------------------------*
if ($upfile eq "")
{
    #--------------------------------------*
    # Intestazione per HTML                *
    #--------------------------------------*
    print "Content-type: text/html\n\n"   ;
    #--------------------------------------*
    # Visualizzazione form                 *
    #--------------------------------------*
    DisplayForm()                         ;
    exit()                                ;
}

#==========================================*
# Ciclo di upload                          *
#------------------------------------------*
my @fhs = $cgi->upload('file_jpg')        ;

my $fil_nam = $prt_bfo . "_" . $prg_bfo . ".jpg";

foreach my $fh (@fhs)
{
    if (defined $fh)
    {
        #----------------------------------*
        # Verifica del tipo di file        *
        #----------------------------------*
        my $type = $cgi->uploadInfo($fh)->{'Content-Type'};
        
        next unless ($type eq "image/jpeg");

        my $io_handle = $fh->handle       ;
        #----------------------------------*
        # Open file di upload              *
        #----------------------------------*
#        open (OUTFILE,'>>', $udir . $fh)  ;
        open (OUTFILE,'>>', $udir . $fil_nam)  ;
        while (my $bytesread = $io_handle->read(my $buffer, 1024))
        {
            print OUTFILE $buffer         ;
        }
        #----------------------------------*
        # Close file di upload             *
        #----------------------------------*
        close (OUTFILE)                   ;
    }
}

#==========================================*
# Parsing del file e scrittura             *
#------------------------------------------*
&pars($udir, $upfile)                     ;

#==========================================*
# FINE SCRIPT                              *
#==========================================*

#==========================================*
# Subroutines                              *
#==========================================*

#==========================================*
# Parsing del file e scrittura             *
#------------------------------------------*
sub pars
{
    #--------------------------------------*
    # Variabili passate                    *
    #--------------------------------------*
    my $fdir = shift                      ;
    my $fnam = shift                      ;
    #--------------------------------------*
    # Variabili di comodo                  *
    #--------------------------------------*
    my $path = $sdir 
             . "/elebfoi0.pl"             ;
    #--------------------------------------*
    # A visualizzazione                    *
    #--------------------------------------*
    print $cgi->redirect($path)           ;
}

#==========================================*
# Visualizza pagina Web                    *
#------------------------------------------*
sub DisplayForm
{
print <<"HTML";

<html>
<head><title>Entrata merce {ELETTRA}</title></head>
<link rel='stylesheet' type='text/css' href='$csss'>
<script src='$jquy'    type='text/javascript'></script>
<script src='$jscs'    type='text/javascript'></script>
<body>
<hr size=3>
<!-- header -->
<table class='bordotab' border='3' width=100%>
<tr align='center'>
<td width=10%>$prt_bfo</td>
<td width=80%>
<center><a href='#'>
<img src='$wdir/icons/wip_2.png' width='100'>
</a></center>
</td>
<td width=10%>$prg_bfo</td>
</tr>
</table>
<!-- -------------------- -->
<!-- FINE header          -->
<!-- -------------------- -->
<hr size=3>
<br>
<br>
<br>
<form id='img_inp' name='img_inp' action='' method='post' enctype='multipart/form-data'>
<center>
<!-- -------------------- -->
<!-- TABELLA              -->
<!-- -------------------- -->
<table class='bordotab' border='1' cellspacing='2' cellpadding='2'>
<tr align='left'>
<td>
<p2>
<!-- -------------------- -->
<!-- CAMPI 'HIDDEN'       -->
<!-- -------------------- -->
<input type='hidden' id='prt_bfo' name='prt_bfo' value='$prt_bfo'>
<input type='hidden' id='prg_bfo' name='prg_bfo' value='$prg_bfo'>
<!-- -------------------- -->
<!-- INSERIMENTO IMMAGINE -->
<!-- -------------------- -->
<input type='file' multiple id='file_jpg' name='file_jpg' accept='.jpeg,.jpg'>
</p2>
</td>
</tr>
<!-- -------------------- -->
<!-- PREVIEW IMMAGINE     -->
<!-- -------------------- -->
<tr>
<td class='td_nb'>
<img id='previewHolder' name='previewHolder' width='200'>
</td>
</tr>
</table>
<br>
<br>
<p1>
<!-- -------------------- -->
<!-- PREVIEW IMMAGINE     -->
<!-- -------------------- -->
<input type='button' name='carica' id='carica' value='CARICA IMMAGINE' />
</p1>
</center>
</form>
<br>
<hr size=3>
<style type="text/css">P1 {font-size : 24px;}</style>
<center><P1><span id='info'></span></P1></center>
<hr size=3>
<br>
<br>
<address><font size=2>TANGRAM &copy - wip</address>
</body>
</html>
HTML
}

