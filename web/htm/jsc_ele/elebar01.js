/*========================================*/
/* elebar01 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 18/05/2023           */
/*========================================*/



// NON UTILIZZATO



/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Tabella accettazione testata       */
    /*====================================*/


    /*====================================*/
    /* Codice prodotto                    */
    /*------------------------------------*/
    $('#acc_ba1').focus()                 ;
    $('#acc_ba1').prop("enabled",true)    ;



    /*====================================*/
    /* Tabella accettazione dati          */
    /*====================================*/

    /*====================================*/
    /* Modifica barcode                   */
    /*------------------------------------*/
    $('.acc_bar').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WB"                ;
        /*--------------------------------*/
        /* Valori da input                */
        /*--------------------------------*/
        var num_pro = $('#num_pro').val();
        var alf_pro = $('#alf_pro').val();
        var ba1_rig = $('#acc_ba1').val();
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_des=" + rsp_des ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "num_pro=" + num_pro ;
        var prm_04 = "alf_pro=" + alf_pro ;
        var prm_05 = "ba1_rig=" + ba1_rig ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05                  ;



        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url : "./elebfow0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Eventuale messaggio    */
                /*------------------------*/
                $('#msg').html(data)      ;
                alert (data)              ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
          //      $("#ubi_001").submit()    ;
            }
            ,error: function(XMLHttpRequest, textStatus, errorThrown)
            {
                console.log('error');
            }
        });







    });          




/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

/*========================================*/
/* FUNZIONI                               */
/*========================================*/

/*========================================*/
/* Funzione generica di switch colore     */
/* cella                                  */
/*----------------------------------------*/
function cel_css (field, swc)
{
    /*------------------------------------*/
    /* Normalizzazione variabile colore   */
    /*------------------------------------*/
    var color = ''                        ;
    /*------------------------------------*/
    /* Colore in funzione dello switch    */
    /*------------------------------------*/
    if (swc == "-")
    {
        color = 'BBBBBB'                  ;
    }
     else if (swc == "+")
    {
        color = '777777'                  ;
    }
     else if (swc == "R")
    {
        color = 'FF0000'                  ;
    }
     else if (swc == "G")
    {
        color = '00FF00'                  ;
    }
     else if (swc == "W")
    {
        color = 'FFFFFF'                  ;
    }
     else
    {
        color = 'BBBBBB'                  ;
    }
    /*------------------------------------*/
    /* Aggiornamento sfondo cella         */
    /*------------------------------------*/
    $("#"+field).css("background-color",
                           "#" + color)   ;
/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}

/*========================================*/
/* Funzione generica determinazione       */
/* valore campo riga                      */
/*----------------------------------------*/
function fld_val (row, fld)
{
    /*------------------------------------*/
    /* Determinazione                     */
    /*------------------------------------*/
    val = $(row).find(fld).val()          ;
    /*------------------------------------*/
    /* Valore in uscita                   */
    /*------------------------------------*/
    return val                            ;
/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}
