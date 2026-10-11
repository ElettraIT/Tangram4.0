/*========================================*/
/* ele_ind - Javascript - by WIP          */
/* -------------------------------------- */
/* Aggiornamento   : 02/07/2024           */
/*========================================*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{

    /*================================================================*/
    /* Definizione box di conferma                                    */
    /*----------------------------------------------------------------*/
    var box_dialog = "Esco da gestione tablet?";
    var currentForm;

    $("#dialog").dialog({
        modal: true,
        bgiframe: true,
        width: 300,
        height: 200,
        autoOpen: false,
//        dialogClass:"myClass",
        title: 'ATTENZIONE'
    });

    /*================================================================*/
    /* Bottone di submit                                              */
    /*----------------------------------------------------------------*/
    $("#logout").click(function(theINPUT){
        /*------------------------------------------------------------*/
        /* Variabili per box di avviso                                */
        /*------------------------------------------------------------*/
        theINPUT.preventDefault()                                     ;
        var theREL = $(this).attr("rel")                              ;
        var theMESSAGE = (theREL == undefined || theREL == '') ? box_dialog : theREL;
        var theICON = '<span class="ui-icon ui-icon-alert" style="float:left; margin:0 7px 0 0;"></span>';
        /*------------------------------------------------------------*/
        /* Box di dialogo                                             */
        /*------------------------------------------------------------*/
        $('#dialog').html('<P>' + theICON + theMESSAGE + '</P>');
	 $('#dialog').dialog('option', 'buttons',
	 {
            /*--------------------------------------------------------*/
            /* Scelta 'SI'                                            */
            /*--------------------------------------------------------*/
            "Si" : function()
            {
                /*----------------------------------------------------*/
                /* Integrazione per submit                            */
                /*----------------------------------------------------*/
                var ids = getUrlParameter('ids');
                $('#menu').append("<input type='hidden' name='ids' value='" + ids + "' />");
                $('#menu').append("<input type='hidden' name='log' value='out' />");
                $('#menu').submit();
            },
            /*--------------------------------------------------------*/
            /* Scelta 'NO'                                            */
            /*--------------------------------------------------------*/
            "No" : function()
            {
                /*----------------------------------------------------*/
                /* Box di dialogo: chiusura                           */
                /*----------------------------------------------------*/
                $(this).dialog("close");
            }
        });
        /*------------------------------------------------------------*/
        /* Box di dialogo: apertura                                   */
        /*------------------------------------------------------------*/
	 $("#dialog").dialog("open");
    });

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*----------------------------------------*/
});

/*========================================*/
/* Funzione per ricavare id sessione      */
/*----------------------------------------*/
function getUrlParameter(sParam) {
    var sPageURL = decodeURIComponent(window.location.search.substring(1)),
        sURLVariables = sPageURL.split('&'),
        sParameterName,
        i;

    for (i = 0; i < sURLVariables.length; i++) {
        sParameterName = sURLVariables[i].split('=');

        if (sParameterName[0] === sParam) {
            return sParameterName[1] === undefined ? true : sParameterName[1];
        }
    }
};

