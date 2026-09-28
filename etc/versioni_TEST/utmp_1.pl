#!/usr/bin/perl -w
use Sys::Utmp;
use POSIX qw(strftime);



my $utmp = Sys::Utmp->new();
my $ctr = 0;

my @usr;

$usr[0] = "master";
$usr[1] = "giorgio";









while ( my $utent =  $utmp->getutent() )
{
    if ( $utent->user_process && $utent->ut_user eq "tangram")
    {
      
        $ctr++;
   
        my $usr = $utent->ut_user;
        my $hst = $utent->ut_host;
        
     #   my $tim = scalar localtime($utent->ut_time);
        
        my $tim = strftime("%H:%M:%S", localtime($utent->ut_time));  # Solo l'ora
        
        
        
        
        print "Numero = " . $ctr . "\n";
        print "Utente = " . $usr . "\n";
        print "Host   = " . $hst . "\n\n";
        print "Time   = " . $tim . "\n\n";
      
      
      
            
      print $utent->ut_id,"\n";
      print $utent->ut_line,"\n";
      print $utent->ut_pid,"\n";
      print $utent->ut_type,"\n";

      
        my $tty = $utent->ut_line;
      
      
        my ($aaa,$bbb,$ccc,$ddd) = split(/\./, $hst); 
  
        my $nip = sprintf ("%03d",
                           $ddd)   ;


        print "TTY    = " . $tty . "\n\n";
        print "Nodo   = " . $nip . "\n\n";
        
        
        print "-" x 20 . "\n";
        
        
    }
}
$utmp->endutent;



my @array = (
    ['A001', 'Descrizione del codice A001'],
    ['A002', 'Descrizione del codice A002'],
    ['B001', 'Descrizione del codice B001'],
);

# Funzione per ottenere la descrizione a partire dal codice
sub get_descrizione {
    my ($codice) = @_;
    
    # Scorriamo l'array per trovare il codice corrispondente
    foreach my $pair (@array) {
        if ($pair->[0] eq $codice) {
            return $pair->[1];  # Restituiamo la descrizione
        }
    }
    
    return "Codice non trovato";  # Se il codice non è presente
}

# Testiamo la funzione
my $codice_ricercato = 'A002';
my $descrizione = get_descrizione($codice_ricercato);

print "Descrizione per il codice $codice_ricercato: $descrizione\n";


