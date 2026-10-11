/*========================================*/
/* eleods02 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 06/06/2023           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*------------------------------------*/
    /* Responsabile                       */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_doc option:selected').text();
    /*------------------------------------*/
    /* Focus                              */
    /*------------------------------------*/

    /*====================================*/
    /* Eventuale switch iniziale campi    */
    /*------------------------------------*/
    if (!rsp_doc || rsp_doc == 0)
    {
        $('#alf_pro').prop("disabled",true);
        $('#rsp_doc').focus()             ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_ute", "R")           ;
    }
     else
    {
        $('#alf_pro').prop("enabled",true);
        cel_css("cel_col", "+")           ;
        $('#alf_pro').focus()             ;
    }

    /*====================================*/
    /* Cambio Operatore                   */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#num_ods').val('')             ;
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_002").submit()            ;
    });          

    /*====================================*/
    /* Cambio Codice prodotto             */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_002").submit()            ;
    });          

    /*====================================*/
    /* Accesso tramite codice prodotto    */
    /*                                    */
    /* ___ DA IMPLEMENTARE ___            */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_ods = $(this).closest("tr").find('#pri').val();
        var cod_alf = $(this).closest("tr").find('#alf').val();
        var ide_row = "#frm_" +
                      pri_ods             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_col = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "alf_pro=" + cod_alf ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01                  ;
        /*--------------------------------*/
        /* Test che sia la colonna del    */
        /* codice prodotto                */
        /*--------------------------------*/
        if (num_col == 10)
        {
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            $.redirect("./eleods02",
                {"alf_pro":cod_alf},
                             "POST",
                             "_self")     ;
        }
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

