/*========================================*/
/* elebfo01 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 14/04/2023           */
/* ___ IN LAVORAZIONE ___                 */
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
    /* Protocollo documento               */
    /*------------------------------------*/
    prt_bfo = $('#num_prt').val()         ;
    /*------------------------------------*/
    /* Responsabile                       */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;


    /*====================================*/
    /* Eventuale switch bottoni           */
    /*------------------------------------*/
    if (!prt_bfo || prt_bfo == "")
    {
        /*--------------------------------*/
        /* Se manca il protocollo         */
        /*--------------------------------*/
        $('#del').prop("disabled",true)   ;
        $('#prn').prop("disabled",true)   ;
    }
     else
    {
        /*--------------------------------*/
        /* Se c'e' il protocollo          */
        /*--------------------------------*/
        $('#del').prop("enabled",true)    ;
        $('#prn').prop("enabled",true)    ;
    }

    /*====================================*/
    /* Eventuale switch iniziale campi    */
    /*------------------------------------*/
    if (!rsp_doc || rsp_doc == "")
    {
        $('#rsp_doc').focus()             ;
        cel_css("cel_ute", "+")           ;
        cel_css("cel_prt", "-")           ;
        $('#num_prt').prop("disabled",true);
        cel_css("cel_pro", "-")           ;
        $('#alf_pro').prop("disabled",true);
    }
     else
    {
        cel_css("cel_ute", "-")           ;
        $('#num_prt').focus()             ;
        cel_css("cel_prt", "+")           ;
        $('#num_prt').prop("enabled",true);
        $('#alf_pro').prop("enabled",true);
    }
    
    /*====================================*/
    /* Definizione box di conferma per la */
    /* cancellazione                      */
    /*------------------------------------*/
    $("#dialog-confirm").dialog({
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        autoOpen: false,
        resizable: false,
        modal: true,
        width: 400,
        height: 200,
        title: 'Cancellazione modifiche',
        /*--------------------------------*/
        /* Bottoni                        */
        /*--------------------------------*/
        buttons:
        {
            /*----------------------------*/
            /* Conferma                   */
            /*----------------------------*/
        "Si": function()
            {
                /*------------------------*/
                /* Chiusura box           */
                /*------------------------*/
                $(this).dialog( "close" ) ;
                /*------------------------*/
                /* Cancellazione          */
                /*------------------------*/
                del_rec(prt_bfo)          ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            },
            /*----------------------------*/
            /* Uscita                     */
            /*----------------------------*/
        "No": function() {
          $( this ).dialog( "close" );
        }
      }
    });

    /*====================================*/
    /* Highlight Operatore                */
    /*------------------------------------*/
    $("#rsp_doc").click(function()
    {
        cel_css("cel_ute", "+")           ;
        cel_css("cel_prt", "-")           ;
        cel_css("cel_pro", "-")           ;
    });
    
    /*====================================*/
    /* Highlight Numero documento         */
    /*------------------------------------*/
    $("#num_prt").click(function()
    {
        cel_css("cel_ute", "-")           ;
        cel_css("cel_prt", "+")           ;
        cel_css("cel_pro", "-")           ;
    });
    
    /*====================================*/
    /* Highlight Prodotto                 */
    /*------------------------------------*/
    $('#alf_pro').click(function()
    {
        cel_css("cel_ute", "-")           ;
        cel_css("cel_prt", "-")           ;
        cel_css("cel_pro", "+")           ;
    });

    /*====================================*/
    /* Forzatura campi numerici           */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function () { 
        this.value = this.value.replace(/[^0-9\.]/g,'');
    });

    /*====================================*/
    /* Eventuale controllo Submit         */
    /*                                    */
    /* ___ DA FARE ___                    */
    /*------------------------------------*/
    $("#bfo_001").submit(function(){
        console.log("Confermato");
    });

    /*====================================*/
    /* Cambio Operatore                   */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#num_prt').val('')         ;
        $('#alf_pro').val('')         ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });          

    /*====================================*/
    /* Cambio Numero protocollo           */
    /*------------------------------------*/
    $('#num_prt').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')         ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });          

    /*====================================*/
    /* Cambio Codice prodotto             */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
       $("#bfo_001").submit()             ;
    });          

    /*====================================*/
    /* Eventuale controllo Submit         */
    /* ___ DA FARE ___                    */
    /*------------------------------------*/
    $("#bfo_001").submit(function()
    {
        /*--------------------------------*/
        /* ___ LOG ___                    */
        /*--------------------------------*/
        console.log("Protocollo = " + prt_bfo)  ;
    });

    /*====================================*/
    /* Bottone cancellazione              */
    /*------------------------------------*/
    $("#del").click(function()
    {
        /*--------------------------------*/
        /* Box di conferma                */
        /*--------------------------------*/
        $("#dialog-confirm").dialog("open");
    });

    /*====================================*/
    /* Bottone stampa                     */
    /*------------------------------------*/
    $("#prn").click(function()
    {
        /*--------------------------------*/
        /* Messaggio di stampa in corso   */
        /*--------------------------------*/
        var msg_stp = "<h1>Esecuzione stampa " +
                      "in corso ...</h1"  ;
        $('#msg').html(msg_stp)           ;
        /*--------------------------------*/
        /* Stampa                         */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo di stampa  */
            /*----------------------------*/
            type: "POST",
            url: "./elebfo1s",
            contentType: "application/x-www-form-urlencoded",
            data: "prt_bfo=" +
                   prt_bfo,
            /*----------------------------*/
            /* Se andata a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data);
            }
        });
        
        
    });


    /*====================================*/
    /* Modifica box di spunta             */
    /*------------------------------------*/
    $('input[type="checkbox"]').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WR"                ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prg_bfo = $(this).closest("tr").find('#prg').val();
        var prr_bfo = $(this).closest("tr").find('#prr').val();
        var nco_bfo = $(this).closest("tr").find('#nco').val();
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var ide_row = "#rw2_" +
                      prr_bfo +
                      "_"     +
                      nco_bfo             ;
        /*--------------------------------*/
        /* Altri valori da riga tabella   */
        /*--------------------------------*/
        var qta_bfo = $(ide_row).find('#qta').val()
        var ubi_rig = $(ide_row).find('#ubi').val()
        var ann_rig = $(ide_row).find('#ann').val();
        
        /*--------------------------------*/
        /* Test su campo spunta           */
        /*--------------------------------*/
        if ($(this).closest("tr").find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var chk_bfo = 'S'             ;
            /*----------------------------*/
            /* Disabilitazione input      */
            /*----------------------------*/
            $(ide_row).find('#qta').prop("disabled",true);
            $(ide_row).find('#ubi').prop("disabled",true);
            $(ide_row).find('#ann').prop("disabled",true);
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var chk_bfo = 'N'             ;
            /*----------------------------*/
            /* Abilitazione input         */
            /*----------------------------*/
            $(ide_row).find('#qta').prop("disabled",false);
            $(ide_row).find('#ubi').prop("disabled",false);
            $(ide_row).find('#ann').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfow0",
            contentType: "application/x-www-form-urlencoded",
            data: "tip_ope=" +
                   tip_ope   +
                 "&rsp_doc=" +
                   rsp_doc   +
                 "&prt_bfo=" +
                   prt_bfo   +
                 "&prg_bfo=" +
                   prg_bfo   +
                 "&prr_bfo=" +
                   prr_bfo   +
                 "&nco_bfo=" +
                   nco_bfo   +
                 "&qta_bfo=" +
                   qta_bfo   +
                 "&ann_rig=" +
                   ann_rig   +
                 "&ubi_rig=" +
                   ubi_rig   +
                 "&chk_bfo=" +
                   chk_bfo,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data)      ;
            }
        });

    });          

    /*====================================*/
    /* Visualizzazione riga aggiuntiva    */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prg_bfo = $(this).closest("tr").find("#prg").val();
        var prr_bfo = $(this).closest("tr").find("#prr").val();
        var nco_bfo = $(this).closest("tr").find('#nco').val();
        var num_col = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var ide_row = "#rw2_" +
                      prr_bfo +
                      "_"     +
                      nco_bfo             ;
        /*--------------------------------*/
        /* Test se colonna progressivo o  */
        /* colonna codice prodotto        */
        /*--------------------------------*/
//        if (num_col == 7 || num_col == 8)
        if (num_col == 8)
        {
            /*----------------------------*/
            /* Visualizzazione riga       */
            /*----------------------------*/
            $(ide_row).toggle()           ;
            
        }
    });

    /*====================================*/
    /* Modifica ubicazione                */
    /*------------------------------------*/
    $('#ubi').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "CU"                ;
        /*--------------------------------*/
        /* Codice ubicazione              */
        /*--------------------------------*/
        var cod_ubi = $('#ubi').val()     ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfow0",
            contentType: "application/x-www-form-urlencoded",
            data: "tip_ope=" +
                   tip_ope   +
                 "&cod_ubi=" +
                   cod_ubi,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data)      ;
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
function cel_css (field, snx)
{
    /*------------------------------------*/
    /* Normalizzazione variabile colore   */
    /*------------------------------------*/
    var color = ''                        ;
    /*------------------------------------*/
    /* Colore in funzione dello switch    */
    /*------------------------------------*/
    if (snx == "-")
    {
        color = 'BBBBBB'                  ;
    }
     else
    {
        color = '777777'                  ;
    }
    /*------------------------------------*/
    /* Aggiornamento sfondo cella         */
    /*------------------------------------*/
    $("#"+field).css("background-color",
                           "#" + color)   ;
}

/*========================================*/
/* Funzione di cancellazione confermata   */
/*----------------------------------------*/
function del_rec (prt)
{
    /*------------------------------------*/
    /* Tipo operazione: cancellazione     */
    /*------------------------------------*/
    var tip_ope = "DE"                    ;
    /*------------------------------------*/
    /* Aggiornamento Tangram              */
    /*------------------------------------*/
    $.ajax({
        /*--------------------------------*/
        /* Richiamo modulo                */
        /*--------------------------------*/
        type: "POST",
        url: "./elebfow0",
        contentType: "application/x-www-form-urlencoded",
        data: "tip_ope=" +
               tip_ope   +
             "&prt_bfo=" +
               prt_bfo,
        /*--------------------------------*/
        /* Se andato a buon fine          */
        /*--------------------------------*/
        success: function (data)
        {
            $('#msg').html(data);
        }
   });
}



