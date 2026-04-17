       Identification Division.
       Program-Id.                                 fbgtel01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    fbgtel              *
      *                    ------------------------------------------- *
      *                       Versione attuale:    001 del 20/09/99    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Aggiornamento file [cli]                    *
      *                                       [dcc]                    *
      *                                       [fnt]                    *
      *                                       [dcf] : Foralberg        *
      *                                                                *
      *                    Pulizia numeri di telefono                  *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.        N-d-K-Sia-PD .
       Object-Computer.        N-d-K-Sia-PD .

       Special-Names.          Decimal-Point     Is Comma .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.
           select print-file
           assign to print "-P SPOOLER-DIRECT"
           line sequential.

           select file-da-stampare
           assign to NomeFile
           organization binary sequential
           access mode is sequential.

       File Section.
       
       fd  print-file.
       01  print-line           pic x(256).

       fd  file-da-stampare.
       01  linea                pic x(256).

       working-storage section.
       01  NomeFile             pic x(80) value "word.prn".
       78  RecLen               value 256.
       01  RecordInteri         pic 9(18).
       01  LunghezzaUltimo      pic 9(3).
       01  file-info.
           02 file-size         pic x(8) comp-x.
           02 file-date         pic 9(8) comp-x.
           02 file-time         pic 9(8) comp-x.
       procedure division.
       main-logic.
           call "C$FILEINFO" using NomeFile, file-info.
           divide RecLen into file-size
                  giving RecordInteri
                  remainder LunghezzaUltimo.
           set environment "STRIP_TRAILING_SPACES" to "1"
                           "MIN_REC_SIZE"          to RecLen.
           open input  file-da-stampare.
           open output print-file.
           display "Stampa in corso".
           perform RecordInteri times
              move spaces to linea
              read file-da-stampare
              move linea to print-line
              write print-line with no control
           end-perform.
           if LunghezzaUltimo > 0
              set environment "MIN_REC_SIZE" to LunghezzaUltimo
              move spaces to linea
              read file-da-stampare
              move linea to print-line
              write print-line with no control
           end-if.
           close file-da-stampare
           close print-file
           stop run.

