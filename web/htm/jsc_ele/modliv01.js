/*========================================*/
/* modliv01 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 15/01/2024           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Variabili globali                  */
    /*------------------------------------*/
    /* Livello operativo                  */
    /*                                    */
    /* - '0' : Accettazione responsabile  */
    /* - '1' : Accettazione protocolli    */
    /* - '2' : Accettazione prodotti      */
    /*------------------------------------*/
    liv_ope = $('#liv_ope').val()         ;

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione elenco 'check'          */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal: true,
        title: 'Scheda prodotto',
        width: 950,
        height: 450,
    });

    /*====================================*/
    /* Deviazione in funzione del livello */
    /*------------------------------------*/
    if (liv_ope == 0 || !liv_ope)
    {
        /*--------------------------------*/
        /* Livello '0'                    */
        /*--------------------------------*/
        liv_ope = 0                       ;
        liv_000 ()                        ;
    }
     else if (liv_ope == 1)
    {
        /*--------------------------------*/
        /* Livello '1'                    */
        /*--------------------------------*/
        liv_001 ()                       ;
    }
     else
    {
        /*--------------------------------*/
        /* Livello '2'                    */
        /*--------------------------------*/
        liv_002 ()                       ;
    }

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
/* Funzione di gestione livello '0'       */
/*                                        */
/* Input Operatore                        */
/*----------------------------------------*/
function liv_000 ()
{
    /*====================================*/
    /* Cella di accettazione in rosso     */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    cel_css("cel_ute", "R")               ;

    /*====================================*/
    /* Cambio Operatore                   */
    /*                                    */
    /* Livello '0'                        */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Se impostato                   */
        /*--------------------------------*/
        if ($(this) != '')
        {
            /*----------------------------*/
            /* A livello successivo       */
            /*----------------------------*/
            $('#liv_ope').val('1')        ;
        }
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });          

/*----------------------------------------*/
/* Fine funzione livello '0'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '1'       */
/*                                        */
/* Input Protocolli                       */
/*----------------------------------------*/
function liv_001 ()
{
    /*====================================*/
    /* Numero documento                   */
    /*------------------------------------*/
    num_doc = $('#num_doc').val()         ;
    /*------------------------------------*/
    /* Responsabile                       */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_doc option:selected').text();
    /*------------------------------------*/
    /* Flag di stampa                     */
    /*------------------------------------*/
    flg_stp = $('#flg_stp').val()         ;

    /*====================================*/
    /* Highlight Numero documento         */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#num_doc').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_num", "+")           ;
    });
    
    /*====================================*/
    /* Cambio Numero documento            */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#num_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });          

    /*====================================*/
    /* Bottone reset                      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#num_doc').val('')             ;
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });

    /*====================================*/
    /* Bottone reset codice prodotto      */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#rep").click(function()
    {
        /*--------------------------------*/
        /* Se prodotto inserito           */
        /*--------------------------------*/
        var alf = $('#alf_pro').val()     ;
        if (alf != '')
        {
            /*----------------------------*/
            /* Normalizzazione livello    */
            /* operativo                  */
            /*----------------------------*/
            $('#liv_ope').val('')         ;
            /*----------------------------*/
            /* Normalizzazione prodotto   */
            /*----------------------------*/
            $('#alf_pro').val('')         ;
            /*----------------------------*/
            /* Caricamento del modulo     */
            /*----------------------------*/
            $("#bfo_001").submit()        ;
        }
    });

    /*====================================*/
    /* Inibizione totale se stampante     */
    /* verde                              */
    /* ___ RIPRISTINARE ___               */
    /*------------------------------------*/
    if (flg_stp != '#')
    {
//        $('input[type=checkbox]').attr('disabled','true');
    }

    /*====================================*/
    /* Eventuale switch bottone stampa    */
    /*------------------------------------*/
    if (flg_stp == "#" || flg_stp == undefined)
    {
        cel_css("stp", "R")               ;
        $('#stp').prop("disabled",true)   ;
    }
     else
    {
        cel_css("stp", "G")               ;
        $('#stp').prop("enabled",true)    ;
    }
    
    /*====================================*/
    /* Eventuale inibizione codice        */
    /* prodotto                           */
    /*------------------------------------*/
    if (!num_doc || num_doc == "")
    {
        $('#alf_pro').prop("disabled",true);
    }
     else
    {
        $('#alf_pro').prop("enabled",true);
    }
    
    /*====================================*/
    /* Focus su prodotto                  */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#alf_pro').focus()                 ;
    $('#alf_pro').select()                ;

    /*====================================*/
    /* Cambio prodotto                    */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Test su codice prodotto        */
        /*--------------------------------*/
        var cod = $('#alf_pro').val();
        if (cod!='')
        {
            /*----------------------------*/
            /* A livello successivo       */
            /*----------------------------*/
            $('#liv_ope').val('2')        ;
        };
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });          

    /*====================================*/
    /* Accettazione quantita' verificata: */
    /* scorciatoia per copiare quantita'  */
    /* ordinata                           */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Numero colonna                 */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var alf_pro = $(ide_row).find('#alf').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = 'S'             ;
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = 'N'             ;
        }
        
        /*================================*/
        /* Test che sia la colonna del    */
        /* codice prodotto                */
        /*--------------------------------*/
        if (num_cln == 15)
        {
            /*----------------------------*/
            /* A livello successivo solo  */
            /* se NON spuntata            */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                /*------------------------*/
                /* Progressivo riga       */
                /*------------------------*/
                $('#prg_bfo').val(prg_bfo);
                /*------------------------*/
                /* Codice prodotto        */
                /*------------------------*/
                $('#alf_pro').val(alf_pro);
                /*------------------------*/
                /* A livello successivo   */
                /*------------------------*/
                $('#liv_ope').val('2')    ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        }

        /*================================*/
        /* Test che sia la colonna della  */
        /* quantita' ordinata e non ci    */
        /* sia gia' la spunta             */
        /*--------------------------------*/
        if (num_cln == 18)
        {
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                var qta_bfo = $(this).closest("tr").find('#qta').val();
                $(this).closest("tr").find('#qtv').val(qta_bfo);
            }
        }
    });

    /*====================================*/
    /* Highlight prodotto                 */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.numbersOnly').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        cel_css("cel_num", "+")           ;
    });

    /*====================================*/
    /* Rilevazione tasto funzione Tangram */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function(e)
    {
        /*--------------------------------*/
        /* Rilevazione tasto              */
        /*--------------------------------*/
        var tgk = (e.keyCode ? 
	            e.keyCode : 
	            e.which)               ;
        if(tgk == 13){$("#bfo_001").submit()};
    });           

    /*====================================*/
    /* Modifica box di spunta o quantita' */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('input[type="checkbox"],.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WC"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var qta_bfo = $(ide_row).find('#qta').val();
        var qtv_bfo = $(ide_row).find('#qtv').val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = 'S'             ;
            /*----------------------------*/
            /* Disabilitazione input      */
            /*----------------------------*/
            $(ide_row).find('#qta').prop("disabled",true);
            $(ide_row).find('#ann').prop("disabled",true);
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = 'N'             ;
            /*----------------------------*/
            /* Abilitazione input         */
            /*----------------------------*/
            $(ide_row).find('#qta').prop("disabled",false);
            $(ide_row).find('#ann').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Messaggio di quantita' a zero  */
        /*--------------------------------*/
        if (qtv_bfo == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_doc=" + rsp_doc ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_bfo=" + prt_bfo ;
        var prm_04 = "prg_bfo=" + prg_bfo ;
        var prm_05 = "pri_bfo=" + pri_bfo ;
        var prm_06 = "qta_bfo=" + qta_bfo ;
        var prm_07 = "qtv_bfo=" + qtv_bfo ;
        var prm_08 = "flg_spn=" + flg_spn ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06 + "&" +
                  prm_07 + "&" +
                  prm_08                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                $('#msg').html(data)      ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Bottone Non conformita'            */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.ncf').click(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_rw2 = "#rw2_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw2).toggle()               ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        $(this).closest("tr").find('#cel_ncf').css({"background-color": "#BBBBBB"});
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var ide_rw4 = "#rw4_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw4).toggle()               ;
        /*--------------------------------*/
        /* Normalizzazione highlight del- */
        /* le righe                       */
        /*--------------------------------*/
        $("#rig td").css({"background-color": "#FFFFFF"});
        /*--------------------------------*/
        /* Highlight cella                */
        /*--------------------------------*/
        $(this).closest("tr").find('#cel_ncf').css({"background-color": "#BBBBBB"});
    });

    /*====================================*/
    /* Accettazione testo Non conformita' */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.inp_ncf').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WN"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#rw2_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var pri_bfo = $(ide_row).find('#pri').val();
        var ann_ncf = $(ide_row).find('#nct').val();
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_des=" + rsp_des ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_bfo=" + prt_bfo ;
        var prm_04 = "prg_bfo=" + prg_bfo ;
        var prm_05 = "pri_bfo=" + pri_bfo ;
        var prm_06 = "ann_ncf=" + ann_ncf ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
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
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Accettazione Barcode               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('.acc_klb').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WB"                ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_bfo = $(this).closest("tr").find('#pri').val();
        var ide_row = "#rw3_" +
                      pri_bfo             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var pri_bfo = $(ide_row).find('#pri').val();
        var klb_pro = $(ide_row).find('#klb').val();
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_des=" + rsp_des ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_bfo=" + prt_bfo ;
        var prm_04 = "prg_bfo=" + prg_bfo ;
        var prm_05 = "pri_bfo=" + pri_bfo ;
        var prm_07 = "klb_pro=" + klb_pro ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        var prm = prm_01 + "&" +
                  prm_02 + "&" +
                  prm_03 + "&" +
                  prm_04 + "&" +
                  prm_05 + "&" +
                  prm_06 + "&" +
                  prm_07                  ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
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
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Bottone stampa                     */
    /*------------------------------------*/
    $("#stp").click(function()
    {
        /*--------------------------------*/
        /* Protocollo                     */
        /*--------------------------------*/
        var prt_bfo = $('#prt').val()     ;
        /*--------------------------------*/
        /* Disabilitazione temporanea     */
        /*--------------------------------*/
//        $('#stp').prop("disabled",true)   ;
        /*--------------------------------*/
        /* Messaggio di stampa in corso   */
        /*--------------------------------*/
        var msg_stp = "<h1>Esecuzione " +
                      "stampa documento " +
                      "in corso ...</h1"  ;
        $('#msg').html(msg_stp)           ;
        /*--------------------------------*/
        /* Attesa di pochi secondi        */
        /*--------------------------------*/
        $('#stp').slideUp(1200).fadeIn(1200);
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
                /*------------------------*/
                /* Esito                  */
                /*------------------------*/
                $('#msg').html(data)      ;
            }
        });
    });

/*----------------------------------------*/
/* Fine funzione livello '1'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '2'       */
/*                                        */
/* Input quantita' per ubicazione         */
/*----------------------------------------*/
function liv_002 ()
{
    /*====================================*/
    /* Focus su ubicazione e quantita'    */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#cod_ubi').focus()                 ;
    $('#cod_ubi').select()                ;

    var ubi = $('#ubi').val()             ;
    if (ubi != '')
    {
            $('.numbersOnly').focus()     ;
            $('.numbersOnly').select()    ;

    }

    /*====================================*/
    /* Bottone fine modifica              */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $('#liv_ope').val('1')            ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });

    /*====================================*/
    /* Bottone refresh                    */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#bfo_001").submit()            ;
    });

    /*====================================*/
    /* Highlight quantita'                */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.numbersOnly').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
    });

    /*====================================*/
    /* Rilevazione tasto funzione Tangram */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function(e)
    {
        /*--------------------------------*/
        /* Rilevazione tasto              */
        /*--------------------------------*/
        var tgk = (e.keyCode ? 
	            e.keyCode : 
	            e.which)               ;
        /*--------------------------------*/
        /* Se 'invio' o 'return'          */
        /*--------------------------------*/
        if(tgk == 13)
          {$("#bfo_001").submit()}        ;
    });           

    /*====================================*/
    /* Modifica quantita'                 */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Codice ubicazione              */
        /*--------------------------------*/
        var cod_ubi = $('#ubi').val()     ;
        /*--------------------------------*/
        /* Test preliminare se ubicazione */
        /* selezionata                    */
        /*--------------------------------*/
        if (cod_ubi =='')
        {
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            alert ("Selezionare una ubicazione");
                $("#bfo_001").submit()    ;
        }
    
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
        /*--------------------------------*/
        /* Numero documento               */
        /*--------------------------------*/
        num_doc = $('#num_doc').val()     ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_ods = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_ods             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var prt_bfo = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var qta_ubi = $(ide_row).find('#qtv').val();
        var num_pro = $(ide_row).find('#pro').val();
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = 'S'             ;
            /*----------------------------*/
            /* Disabilitazione input      */
            /*----------------------------*/
            $(ide_row).find('#qtv').prop("disabled",true);
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = 'N'             ;
            /*----------------------------*/
            /* Abilitazione input         */
            /*----------------------------*/
            $(ide_row).find('#qtv').prop("disabled",false);
        }

        /*--------------------------------*/
        /* Messaggio di quantita' a zero  */
        /*--------------------------------*/
        if (qta_ubi == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        const par = []                    ;
        const val = []                    ;
        /*--------------------------------*/
        par[0] = "rsp_doc"                ;
        val[0] = rsp_doc                  ;
        /*--------------------------------*/
        par[1] = "tip_ope"                ;
        val[1] = tip_ope                  ;
        /*--------------------------------*/
        par[2] = "prt_bfo"                ;
        val[2] = num_doc                  ;
        /*--------------------------------*/
        par[3] = "prg_bfo"                ;
        val[3] = prg_bfo                  ;
        /*--------------------------------*/
        par[4] = "cod_ubi"                ;
        val[4] = cod_ubi                  ;
        /*--------------------------------*/
        par[5] = "num_pro"                ;
        val[5] = num_pro                  ;
        /*--------------------------------*/
        par[6] = "qta_ubi"                ;
        val[6] = qta_ubi                  ;

        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
        
        
//        console.log(prm);
        
        
        
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* A livello precedente   */
                /*------------------------*/
 //               $('#liv_ope').val('2')    ;
        
        
   //     console.log(data);
        
        
        
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Accettazione quantita' verificata: */
    /* scorciatoia per copiare quantita'  */
    /* ordinata                           */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
        /*--------------------------------*/
        /* Numero documento               */
        /*--------------------------------*/
        num_doc = $('#num_doc').val()     ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_ods = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_ods             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var num_doc = $(ide_row).find('#prt').val();
        var prg_bfo = $(ide_row).find('#prg_bfo').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var num_pro = $(ide_row).find('#pro').val();
        
        
        var num_cln = $(this).parent().children().index($(this));
        /*--------------------------------*/
        /* Campo di spunta                */
        /*--------------------------------*/
        if ($(ide_row).find('input[type="checkbox"]').is(":checked"))
        {
            /*----------------------------*/
            /* Spunta a 'SI'              */
            /*----------------------------*/
            var flg_spn = 'S'             ;
        }
         else
        {
            /*----------------------------*/
            /* Spunta a 'NO'              */
            /*----------------------------*/
            var flg_spn = 'N'             ;
        }

        /*--------------------------------*/
        /* Test che sia la colonna della  */
        /* quantita' ordinata e non ci    */
        /* sia gia' la spunta             */
        /*--------------------------------*/
        if (num_cln == 11)
        {
        
            /*----------------------------*/
            /* Test se ubicazione         */
            /*----------------------------*/
            if (cod_ubi =='')
            {
                alert ("Selezionare una ubicazione");
                $("#bfo_001").submit()    ;
            }
        
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            var qta_sub = $(this).closest("tr").find('#qta').val();
            $(this).closest("tr").find('#qtv').val(qta_sub);

            
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        const par = []                    ;
        const val = []                    ;
        /*--------------------------------*/
        par[0] = "rsp_doc"                ;
        val[0] = rsp_doc                  ;
        /*--------------------------------*/
        par[1] = "tip_ope"                ;
        val[1] = tip_ope                  ;
        /*--------------------------------*/
        par[2] = "prt_bfo"                ;
        val[2] = num_doc                  ;
        /*--------------------------------*/
        par[3] = "prg_bfo"                ;
        val[3] = prg_bfo                  ;
        /*--------------------------------*/
        par[4] = "cod_ubi"                ;
        val[4] = cod_ubi                  ;
        /*--------------------------------*/
        par[5] = "num_pro"                ;
        val[5] = num_pro                  ;
        /*--------------------------------*/
        par[6] = "qta_ubi"                ;
        val[6] = qta_sub                  ;

        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
        
        
  //      console.log(prm);
        
        
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./elebfox0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* A livello precedente   */
                /*------------------------*/
 //               $('#liv_ope').val('2')    ;
 
        
 //        console.log(data);
        
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#bfo_001").submit()    ;
            }
        });
            
            
            
        }

    });

    /*====================================*/
    /* Cambio ubicazione                  */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#cod_ubi').change(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var cod_ubi = $('#cod_ubi').val() ;
        var num_pro = $('#num_pro').val() ;
        var prg_bfo = $('#prg_bfo').val() ;
        
        
        
        
        
        
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        const par = []                    ;
        const val = []                    ;
        /*--------------------------------*/
        par[0] = "cod_ubi"                ;
        val[0] = cod_ubi                  ;
        /*--------------------------------*/
        par[1] = "num_pro"                ;
        val[1] = num_pro                  ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
        /*--------------------------------*/
        /* Lettura                        */
        /*--------------------------------*/
        $.ajax({ type: 'POST',
                  url: "./eleodsu0",
          contentType: "application/x-www-form-urlencoded",
                 data: prm,
              success: function(data)
              {
                /*------------------------*/
                /* Esito lettura          */
                /*------------------------*/
                let res = data.substring(0, 2);
                /*------------------------*/
                /* Messaggio di errore    */
                /*------------------------*/
                if (res == 'OK')
                {
                    /*--------------------*/
                    /* Progressivo riga   */
                    /*--------------------*/
                    $('#prg_bfo').val(prg_bfo);
                    /*--------------------*/
                    /* Esecuzione modulo  */
                    /*--------------------*/
                    $("#bfo_001").submit()
                }
                 else if (res == 'KU')
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    alert ('Ubicazione nuova');
                    /*--------------------*/
                    /* Progressivo riga   */
                    /*--------------------*/
                    $('#prg_bfo').val(prg_bfo);
                    /*--------------------*/
                    /* Esecuzione modulo  */
                    /*--------------------*/
                    $("#bfo_001").submit()
                }
                 else
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    alert ('Ubicazione inesistente');
                    /*--------------------*/
                    /* Normalizzazione    */
                    /* Ubicazione         */
                    /*--------------------*/
                    $('#cod_ubi').val('');
                }
            } 
        });    
    });          

/*----------------------------------------*/
/* Fine funzione livello '2'              */
/*========================================*/
}

/*========================================*/
/* Funzione per assemblaggio parametri    */
/*----------------------------------------*/
function ass_prm(nom, val)
{
    /*------------------------------------*/
    /* Normalizzazioni                    */
    /*------------------------------------*/
    let inx = 0                           ;
    let par = ''                          ;
    /*------------------------------------*/
    /* Ciclo                              */
    /*------------------------------------*/
    while (inx < nom.length)
    {
        /*--------------------------------*/
        /* Assemblaggio                   */
        /*--------------------------------*/
        par = par 
            + nom[inx]
            + "=" 
            + val[inx]
            + "&"                         ;
        /*--------------------------------*/
        /* Incremento indice              */
        /*--------------------------------*/
        inx++;
    }        
    /*------------------------------------*/
    /* Valore in uscita                   */
    /*------------------------------------*/
    return par                            ;
/*----------------------------------------*/
/* Funzione richiesta inserimento - fine  */
/*========================================*/
}


