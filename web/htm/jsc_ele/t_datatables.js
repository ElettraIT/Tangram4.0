/*========================================*/
/* File : t_datatables.js         (C) wip */
/*                                        */
/* Esportazioni standard : 09/02/2022     */
/*                                        */
/* Caricato da 'mxport'                   */
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
    /* Personalizzazioni per DataTable    */
    /*------------------------------------*/
    $('#mxport').DataTable({
        /*--------------------------------*/
        /* Definizioni per numero righe   */
        /* per pagina                     */
        /*--------------------------------*/
        dom: 'Bfrtip',
        lengthMenu: [
            [ 10, 25, 50, -1 ],
            [ '10 righe', '25 righe', '50 righe', 'Mostra tutte' ]
        ],
        buttons: [
            'pageLength', 'copy', 'csv', 'excel', 'print'
            
        ],

        /*--------------------------------*/
        /* Definizioni per Italiano       */
        /*--------------------------------*/
        language:
        {
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
            infoFiltered  : "(Filtrato da _MAX_  records)",
            infoPostFix   : "",
            loadingRecords: "Caricamento in corso ...",
            zeroRecords   : "Nessun elemento da mostrare",
            emptyTable    : "Nessun elemento in tabella",
            paginate      : 
            {
                first     : "Prima",
                previous  : "Precedente",
                next      : "Successiva",
                last      : "Ultima"
            },
            /*----------------------------*/
            /* Definizioni per Italiano   */
            /* - bottoni                  */
            /*----------------------------*/
            buttons:
            {
                copy      : "Copia",
                print     : "Stampa",
                pageLength:
                {
                       _: "Mostra %d righe",
                    '-1': "Mostra tutto"
                }
            }
        }
    });

/*----------------------------------------*/
/* Controllo documento - fine             */
/*========================================*/
});
