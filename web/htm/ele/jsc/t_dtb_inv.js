/*========================================*/
/* t_dtb_inv.js - Javascript - by A&N     */
/*                                        */
/* Personalizzazioni per Datatables       */
/*                                        */
/* ELETTRA : Controllo inventario         */
/* -------------------------------------- */
/* Aggiornamento   : 18/11/2024           */
/*========================================*/

/*========================================*/
/* Controllo documento                    */
/*----------------------------------------*/
$(document).ready(function()
{

    /*====================================*/
    /* Formattazione date                 */
    /*------------------------------------*/
    $.fn.dataTable.moment( 'DD/MM/YYYY' );

    /*====================================*/
    /* Definizione di orderDataType per   */
    /* interpretare i checkbox            */
    /*------------------------------------*/
    $.fn.dataTable.ext.order['dom-checkbox'] = function(settings, colIndex) {
        /*--------------------------------*/
        /* Verifica lo stato del checkbox */
        /* (checked o unchecked) e lo     */
        /* converte in 1 o 0              */
        /*--------------------------------*/
        return settings.aoData.map(function(row) {
            return $('input[type="checkbox"]', row.anCells[colIndex]).prop('checked') ? 1 : 0;
        });
    };
    
    /*====================================*/
    /* Personalizzazioni per DataTable    */
    /*------------------------------------*/
    $('#mxport').DataTable({
        /*--------------------------------*/
        /* Definizioni posizione nel DOM  */
        /* dei vari pulsanti e ricerca    */
        /*--------------------------------*/
        dom: '<"top-left"B><"top-right"f>rt<"bottom"p><"clear">',

        /*--------------------------------*/
        /* Mostra tutte le righe della    */
        /* tabella                        */
        /*--------------------------------*/
        //pageLength: -1,

        /*--------------------------------*/
        /* Mostra la navigazione tra le   */
        /* pagine solo se piu' di una     */
        /*--------------------------------*/
        fnDrawCallback: function() {
            /*----------------------------*/
            /* Ottieni il numero di righe */
            /* visibili dopo i filtri     */
            /*----------------------------*/
            var rows_visible = this.api().rows({ filter: 'applied' }).data().length;
            /*----------------------------*/
            /* Mostra/nascondi la         */
            /* paginazione in base al     */
            /* numero di righe            */
            /*----------------------------*/
            if (rows_visible <= 10) {
                /*------------------------*/
                /* Nasconde l'            */
                /* impaginazione          */
                /*------------------------*/
                $('div.dt-paging.paging_full_numbers').hide();
            } else {
                /*------------------------*/
                /* Mostra l'              */
                /* impaginazione          */
                /*------------------------*/
                $('div.dt-paging.paging_full_numbers').show();
            }
        },
                        
        /*--------------------------------*/
        /* Definizioni per numero righe   */
        /* per pagina                     */
        /*--------------------------------*/
        lengthMenu: [
            [ 10, 25, 50, -1 ],
            [ '10 righe', '25 righe', '50 righe', 'Mostra tutte' ]
        ],
        /*--------------------------------*/
        /* Configurazione per le colonne  */
        /*--------------------------------*/
        columnDefs: [
            {
                /*------------------------*/
                /* Applica a tutte le     */
                /* colonne                */
                /*------------------------*/
                targets: '_all',
                orderSequence: ['asc', 'desc']
            },
            { targets: 0, orderable: true },
            { targets: 1, orderable: true, type: 'alphanumeric' },
            { targets: 2, orderable: true, type: 'alphanumeric' },
            { targets: 3, orderable: false},
            { targets: 4, orderable: true },
            { targets: 5, orderable: false},
            { targets: 6, orderable: true },
            { targets: 7, orderable: true },
            { targets: 10, orderable: false },
            { targets: 11, orderable: false },

            {
            /*----------------------------*/
            /* Colonna checkbox chk_spn   */
            /*----------------------------*/
            targets: 8,
            orderable: true,
            type: 'num', 
            render: function(data, type, row) {
                /*------------------------*/
                /* Usa 1 o 0 per          */
                /* l'ordinamento,         */
                /* mantiene il checkbox   */
                /* visibile  e            */
                /* interattivo per la     */
                /* visualizzazione        */
                /*------------------------*/
                if (type === 'display') {
                    return data;
                }
                return $(data).prop('checked') ? 1 : 0;
            }
            },
            {
            /*----------------------------*/
            /* Colonna checkbox chk_cpv   */
            /*----------------------------*/
            targets: 9,
            orderable: true,
            type: 'num',
            render: function(data, type, row) {
                /*------------------------*/
                /* Usa 1 o 0 per          */
                /* l'ordinamento,         */
                /* mantiene il checkbox   */
                /* visibile  e            */
                /* interattivo per la     */
                /* visualizzazione        */
                /*------------------------*/
                if (type === 'display') {
                    return data;
                }
                return $(data).prop('checked') ? 1 : 0;
            }
            }
        ],
        /*--------------------------------*/
        /* Bottoni configurazione         */
        /*--------------------------------*/
        buttons: [
            'pageLength', 'copy', 'csv', 'excel',
            {
            /*----------------------------*/
            /* Configurazione pulsante    */
            /* stampa                     */
            /*----------------------------*/
            extend: 'print',
            text: 'Stampa',
            exportOptions: {
                /*------------------------*/
                /* Esporta solo il        */
                /* contenuto visibile     */
                /* dei campi              */
                /*------------------------*/
                stripHtml: false
            },
            customize: function (win) {
                /*------------------------*/
                /* Personalizza il formato*/
                /* per la stampa          */
                /*------------------------*/
                /*------------------------*/
                /* Sostituisci ogni campo */
                /* checkbox con un spunta */
                /*------------------------*/
                $(win.document.body).find('input[type="checkbox"]').each(function(){
                    /*--------------------*/
                    /* Se checkbox        */
                    /* selezionato mostra */
                    /* 'V' altrimenti     */
                    /* mostra una casella */
                    /* vuota              */
                    /*--------------------*/
                    var chk_val = $(this).prop('checked');
                    $(this).replaceWith(chk_val ? 'V' : ' ');
                });

                /*------------------------*/
                /* Trova tutti i campi di */
                /* input e li sostituisce */
                /* con il loro valore     */
                /*------------------------*/
                $(win.document.body).find('input').each(function(){
                    /*--------------------*/
                    /* Sostituisce input  */
                    /* con valore in sola */
                    /* lettura            */
                    /*--------------------*/
                    var inp_val = $(this).val();
                    $(this).replaceWith(inp_val);
                });
            }
            }
    
        ],
        
        /*--------------------------------*/
        /* Scelta visualizzazione pulsanti*/
        /*--------------------------------*/
        pagingType: 'full_numbers',

        /*--------------------------------*/
        /* Definizioni per Italiano       */
        /*--------------------------------*/
        language: {
            /*----------------------------*/
            /* Numeri in formato europeo  */
            /*----------------------------*/
            decimal       : ",",
            thousands     : ".",
            
            /*----------------------------*/
            /* Prompt di elaborazione     */
            /*----------------------------*/
            processing    : "Caricamento ...",
            search        : "Ricerca : ",
            
            /*----------------------------*/
            /* Prompt risultati           */
            /*----------------------------*/
            lengthMenu    : "Visualizza _MENU_ records per pagina",
            info          : "",
            infoEmpty     : "Nessun record",
            infoFiltered  : "(Filtrato da _MAX_ records)",
            infoPostFix   : "",
            loadingRecords: "Caricamento in corso ...",
            zeroRecords   : "Nessun elemento da mostrare",
            emptyTable    : "Nessun elemento in tabella",

            /*----------------------------*/
            /* Paginazione                */
            /*----------------------------*/
            paginate      : {
                first     : "Prima",
                previous  : "Precedente",
                next      : "Successiva",
                last      : "Ultima"
            },

            /*----------------------------*/
            /* Definizioni per bottoni    */
            /*----------------------------*/
            buttons: {
                copy      : "Copia",
                print     : "Stampa",
                pageLength: {
                    _: "Mostra %d righe",
                    '-1': "Mostra tutto"
                }
            }
        }
    });

    /*====================================*/
    /* Definizione ordinamento            */
    /* personalizzato per DataTable       */
    /*------------------------------------*/

    /*------------------------------------*/
    /* Ordinamento campi checkbox         */
    /*------------------------------------*/
    $.fn.dataTable.ext.order['dom-checkbox'] = function(settings, colIndex) {
        /*--------------------------------*/
        /* Ottiene i valori dei checkbox  */
        /* nella colonna specificata e    */
        /* restituisce 1 se selezionato,  */
        /* 0 se non selezionato           */
        /*--------------------------------*/
        return settings.aoData.map(function(row) {
            /*----------------------------*/
            /* Ottiene il checkbox dalla  */
            /* cella nella colonna        */
            /*----------------------------*/
            var checkbox = $(row.anCells[colIndex]).find('input.chk_spn, input.chk_spv');
            /*----------------------------*/
            /* Se il checkbox è           */
            /* selezionato restituisce 1  */
            /* altrimenti 0               */
            /*----------------------------*/
            return checkbox.prop('checked') ? 1 : 0;
        });
    };

/*----------------------------------------*/
/* Controllo documento - fine            
/*========================================*/
});
