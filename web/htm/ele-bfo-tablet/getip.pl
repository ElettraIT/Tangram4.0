#!/usr/bin/perl -w
#==========================================*
# Modulo Perl per determinazione indirizzo *
# IP del client                            *
#                                          *
# ---------------------------------------- *
# Versione originale:   001 del 09/07/2024 *
#   Ultima revisione:   wip del 09/07/2024 *
# ---------------------------------------- *
#             Autore:   Nicola de Kunovich *
# ---------------------------------------- *
#                                          *
# SU MISURA PER ELETTRA                    *
#                                          *
#==========================================*
#==========================================*
# Intestazione per HTML                    *
#------------------------------------------*
print "Content-type: text/html\n\n"       ;
#==========================================*
# Valore in uscita                         *
#------------------------------------------*
print $ENV{REMOTE_ADDR}                   ;







