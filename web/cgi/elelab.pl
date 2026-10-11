#!/usr/bin/perl -w
#==========================================*
# Stampa etichette per stampanti ZEBRA con *
# interfaccia PDF                          *
#                                          *
# Su misura per ELETTRA                    *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 24/07/2023 *
#   Ultima revisione:   wip del 24/07/2023 *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
# Parametri in input : - Tipo etichette    *
#                      - Nr.  etichette    *
#                      - codice stampante  *
#                      - numero documento  *
#                      - data   documento  *
#                      - [Dati].nnn        *
#                                          *
#   ___ IN LAVORAZIONE ___                 *
#                                          *
#==========================================*

#./elelab.pl 1#1#pdf#1#24/07/2023#RAGIONE_SOCIALE#INDIRIZZO#LOCALITA


#==========================================*
# Moduli Perl                              *
#------------------------------------------*
use PostScript::Simple                    ;

#==========================================*
# Valori in input                          *
#------------------------------------------*
my $val_inp = shift                       ;
my @val_spl = split(/#/, $val_inp)        ;



my $tip_eti = $val_spl[0]                 ;
my $num_eti = $val_spl[1]                 ;
my $cod_stp = $val_spl[2]                 ;
my $num_doc = $val_spl[3]                 ;
my $dat_doc = $val_spl[4]                 ;

my @val_str                               ;
my $max_val = 15                          ;
my $ctr_001                               ;
my $ctr_002                               ;

for ($ctr_001 = 5 ; $ctr_001 < $max_val ; $ctr_001++ )
{



    if ($val_spl[$ctr_001])
    {
        $ctr_002++;

    #  print "val_spl[$ctr_001] = " . $val_spl[$ctr_001] . "\n"    ;

        
         $val_str[$ctr_002] = $val_spl[$ctr_001];
         $val_str[$ctr_002] =~ s/^\s+//        ;
    }
}

    #======================================*
    # DEBUG                                *
    #--------------------------------------*
    #print "tip_eti = " . $tip_eti . "\n"    ;
    #print "num_eti = " . $num_eti . "\n"    ;
    #print "cod_stp = " . $cod_stp . "\n"    ;
    #print "num_doc = " . $num_doc . "\n"    ;
    #print "dat_doc = " . $dat_doc . "\n"    ;





#------------------------------------------*
# Percorsi file                            *
#------------------------------------------*
my $pth_tmp = "/abd/spl/"                 ;
my $out_stp = $pth_tmp . $num_doc . ".ps" ;
my $out_pdf = $pth_tmp . $num_doc . ".pdf";



#==========================================*
# Preparazione pagina                      *
#------------------------------------------*
my $ps = new PostScript::Simple::(units => "mm",
                                  xsize => "270",
                                  ysize => "270",
                                 colour => 1,
                                   clip => 1,
                                    eps => 0);


    #--------------------------------------*
    # Nuova pagina                         *
    #--------------------------------------*
    $ps->newpage                          ;
    #--------------------------------------*
    # Box pagina                           *
    #--------------------------------------*
    $ps->box(5, 5, 90, 90)                ;



    #--------------------------------------*
    # Sottolineatura titolo                *
    #--------------------------------------*
    $ps->line(5, 80, 90, 80);


    #--------------------------------------*
    # Font                                 *
    #--------------------------------------*
        $ps->setfont("Helvetica-Bold", 12)                     ;
    #--------------------------------------*
    # Titolo                               *
    #--------------------------------------*
    $ps->text(10, 85, $dat_doc)       ;



    #--------------------------------------*
    # Titolo                               *
    #--------------------------------------*
    $ps->text(10, 65, "DESTINATARIO")       ;
    $ps->text(10, 60, $val_str[1])       ;
    $ps->text(10, 50, $val_str[2])       ;


    #--------------------------------------*
    # Inclusione barcode EPS               *
    #--------------------------------------*
    $ps->importepsfile("/abd/asc/bcd/01/010100.eps",
                       12,
                       12,
                       100,
                       100)          ;



#------------------------------------------*
# Generazione file Postscript              *
#------------------------------------------*
$ps->output($out_stp)                     ;

#===============================================================*
# Stampa file Postscript                                        *
#---------------------------------------------------------------*
if ($cod_stp eq "pdf")
{
#    system("ps2pdf $out_stp $out_pdf")    ;
    
    system("ps2pdf -dEPSCrop $out_stp $out_pdf")    ;
    
    
    
}
 else
{
    system("cat $out_stp | lpr -P$cod_stp");
}

