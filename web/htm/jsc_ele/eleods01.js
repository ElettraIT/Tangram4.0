/*========================================*/
/* eleods01 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 04/01/2024           */
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
     else if (liv_ope == 2)
    {
        /*--------------------------------*/
        /* Livello '2'                    */
        /*--------------------------------*/
        liv_002 ()                       ;
    }
     else
    {
        /*--------------------------------*/
        /* Livello '3'                    */
        /*--------------------------------*/
        liv_003 ()                       ;
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
        $("#ods_001").submit()            ;
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
    /* Focus su protocollo                */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#prt_ods').focus()                 ;
    $('#prt_ods').select()                ;

    /*====================================*/
    /* Highlight protocollo               */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#prt_ods').click(function()
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
    /* Cambio protocollo                  */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#prt_ods').change(function()
    {
        /*--------------------------------*/
        /* Eliminazione zeri in testa     */
        /*--------------------------------*/
        var prt = $('#prt_ods').val()     ;
            prt = Number(prt).toString()  ;    
            $('#prt_ods').val(prt)        ;

        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });          

    /*====================================*/
    /* Bottone conferma protocolli        */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
        /*--------------------------------*/
        /* Test se presente distinta      */
        /*--------------------------------*/
        var dst = $("#num_dst").val()     ;
        if (dst != '')
        {
            /*----------------------------*/
            /* A livello successivo       */
            /*----------------------------*/
            $('#liv_ope').val('2')        ;
            /*----------------------------*/
            /* Caricamento del modulo     */
            /*----------------------------*/
            $("#ods_001").submit()        ;
        }
    });
    
/*----------------------------------------*/
/* Fine funzione livello '1'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '2'       */
/*                                        */
/* Input Prodotti                         */
/*----------------------------------------*/
function liv_002 ()
{
    /*====================================*/
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*------------------------------------*/
    $('#dialog-confirm').dialog({
      autoOpen: false,
      resizable: false,
      height: "auto",
      width: 400,
      modal: true,
      buttons: {
        "Vuoi spuntare tutti le righe rimanenti?": function() {
          $( this ).dialog( "close" );
        },
        Cancel: function() {
          $( this ).dialog( "close" );
        }
      },

        /*--------------------------------*/
        /* Default 'NO'                   */
        /* ___ NON FUNZIONA ___           */
        /*--------------------------------*/
      open: function() {
          $('#dialog-confirm.ui-dialog-buttonpane button:eq(1)').focus();
          }


    });    

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione ubicazioni              */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $("#dialog-ubi").dialog({
        autoOpen: false,
        modal: true,
        title: 'Scheda prodotto',
        width: 950,
        height: 450,
    });

    /*====================================*/
    /* Definizione box di dialogo -       */
    /* opzioni                            */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#dialog-confirm').dialog('option', 'buttons',
    {
        /*--------------------------------*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        "si" : function()
        {
            /*----------------------------*/
            /* Responsabile               */
            /*----------------------------*/
            var rsp_doc = $('#rsp_doc').val();
            /*----------------------------*/
            /* Tipo operazione            */
            /*----------------------------*/
            var tip_ope = "WG"           ;
            /*----------------------------*/
            /* Numero distinta            */
            /*----------------------------*/
            var num_dst = $('#num_dst').val();
            /*----------------------------*/
            /* Parametri                  */
            /*----------------------------*/
            var prm_01 = "rsp_doc=" + rsp_doc ;
            var prm_02 = "tip_ope=" + tip_ope ;
            var prm_03 = "num_dst=" + num_dst ;
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            var prm = prm_01 + "&" +
                      prm_02 + "&" +
                      prm_03              ;
            /*----------------------------*/
            /* Aggiornamento Tangram      */
            /*----------------------------*/
            $.ajax({
                /*------------------------*/
                /* Richiamo modulo        */
                /*------------------------*/
                type: "POST",
                url: "./eleodsw0",
                contentType: "application/x-www-form-urlencoded",
                data: prm,
                /*------------------------*/
                /* Se andato a buon fine  */
                /*------------------------*/
                success: function (data)
                {
                    /*--------------------*/
                    /* Messaggi           */
                    /*--------------------*/
                    $('#msg').html(data)  ;
                    /*--------------------*/
                    /* Ricarica modulo    */
                    /*--------------------*/
                    $("#ods_001").submit();
                }
            });
        },
        /*--------------------------------*/
        /* Scelta 'NO'                    */
        /*--------------------------------*/
        "NO" : function()
        {
            /*----------------------------*/
            /* Box di dialogo: close      */
            /*----------------------------*/
            $(this).dialog("close")       ;
        }
    });

    /*====================================*/
    /* Focus su prodotto                  */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#alf_pro').focus()                 ;
    $('#alf_pro').select()                ;

    /*====================================*/
    /* Cambio prodotto                    */
    /*                                    */
    /* Livello '2'                        */
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
            $('#liv_ope').val('3')        ;
        };
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });          

    /*====================================*/
    /* Cambio Prodotto da verificare      */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#cod_pdv').change(function()
    {
        /*--------------------------------*/
        /* Codice prodotto selezionato    */
        /*--------------------------------*/
        var pdv = $('#cod_pdv option:selected').text();
        $('#alf_pro').val(pdv)            ;
        var cod = $('#alf_pro').val()     ;
        if (cod!='')
        {
            /*----------------------------*/
            /* Gestione quantita' per     */
            /* ubicazione                 */
            /*----------------------------*/
            $('#liv_ope').val('3')        ;
        };
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });          

    /*====================================*/
    /* Info                               */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_inf').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        var num_dst = $('#num_dst').val();
        var num_dsp = num_dst.substr (3, 8);
            num_dsp = parseInt(num_dsp, 10);
        /*--------------------------------*/
        /* Riempimento box                */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url: "./eleods0d",
            contentType: "application/x-www-form-urlencoded",
            data: "num_dst=" + num_dst,
            success: function (data)
            {
                  $("#dialog").html(data);
                  $("#dialog").dialog("option","title","Check nr. " + num_dsp).dialog('open');
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
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_ods = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_ods             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));
        var alf_pro = $(this).closest("tr").find('#alf').val();
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
        if (num_cln == 10)
        {
            /*----------------------------*/
            /* A livello successivo solo  */
            /* se NON spuntata            */
            /*----------------------------*/
            if (flg_spn == 'N')
            {


        $('#alf_pro').val(alf_pro)       ;
        
        
            /*----------------------------*/
            /* A livello successivo       */
            /*----------------------------*/
            $('#liv_ope').val('3')        ;
            /*----------------------------*/
            /* Ricarica modulo            */
            /*----------------------------*/
            $("#ods_001").submit()        ;


            }
        }

        /*--------------------------------*/
        /* Test che sia la colonna della  */
        /* quantita' ordinata e non ci    */
        /* sia gia' la spunta             */
        /*--------------------------------*/
        if (num_cln == 14)
        {
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                var qta_sub = $(this).closest("tr").find('#qta').val();
                $(this).closest("tr").find('#qtv').val(qta_sub);
            }
        }

    });

    /*====================================*/
    /* Highlight prodotto                 */
    /*                                    */
    /* Livello '2'                        */
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
    /* Livello '3'                        */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function(e)
    {
        /*--------------------------------*/
        /* Rilevazione tasto              */
        /*--------------------------------*/
        var tgk = (e.keyCode ? 
	            e.keyCode : 
	            e.which)               ;
        if(tgk == 13){$("#ods_001").submit()};
    });           

    /*====================================*/
    /* Modifica box di spunta o quantita' */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('input[type="checkbox"],.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WR"                ;
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
        var prt_ods = $(ide_row).find('#prt').val();
        var prg_ods = $(ide_row).find('#prg').val();
        var qta_ods = $(ide_row).find('#qta').val();
        var qtv_ods = $(ide_row).find('#qtv').val();
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
        if (qtv_ods == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "rsp_doc=" + rsp_doc ;
        var prm_02 = "tip_ope=" + tip_ope ;
        var prm_03 = "prt_ods=" + prt_ods ;
        var prm_04 = "prg_ods=" + prg_ods ;
        var prm_05 = "pri_ods=" + pri_ods ;
        var prm_06 = "qta_ods=" + qta_ods ;
        var prm_07 = "qtv_ods=" + qtv_ods ;
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
                  
                  
                  console.log(prm);
                  
                  
        /*--------------------------------*/
        /* Verifica righe nr spuntate     */
        /*--------------------------------*/
        var rig = 0                       ;
        var spt = 0                       ;
        var nsp = 0                       ;
        $("#rig tr").each(function()
        {
            var unc = $(this).find('input[type="checkbox"]:not(:checked)');
            if (unc.length)
            {nsp++} else {spt++}          ;
            rig++                         ;
        });                  
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleodsw0",
            contentType: "application/x-www-form-urlencoded",
            data: prm,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                
                
                  console.log(data);
                
                
                /*------------------------*/
                /* Focus su prodotto      */
                /*------------------------*/
                $('#alf_pro').focus()     ;
                /*------------------------*/
                /* Ricarica modulo solo   */
                /* se le righe sono tutte */
                /* spuntate               */
                /*------------------------*/
                if (spt == rig)
                {
                    $("#ods_001").submit();
                }
            }
        });
    });          

    /*====================================*/
    /* Bottone conferma protocolli        */
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
        /* A livello precedente           */
        /*--------------------------------*/
        $('#num_dst').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
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
        $("#ods_001").submit()            ;
    });
    
    /*====================================*/
    /* Bottone completamento spunte       */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_gen').click(function()
    {
        /*--------------------------------*/
        /* Calcolo righe da spuntare      */
        /*--------------------------------*/
        var rig = 0                       ;
        var spt = 0                       ;
        var nsp = 0                       ;
        $("#rig tr").each(function()
        {
            var unc = $(this).find('input[type="checkbox"]:not(:checked)');
            if (unc.length)
            {nsp++} else {spt++}          ;
            rig++                         ;
        });                  
        var dsp = rig - spt               ;
        /*--------------------------------*/
        /* Se zero righe da spuntare      */
        /*--------------------------------*/
        if (dsp == 0){return;}            ;
        /*--------------------------------*/
        /* Messaggio conferma operazione  */
        /*--------------------------------*/
        var res_msg = ''                  ;
        
        if (dsp == 1)
        {
            res_msg = '<h1>Vuoi veramente spuntare la riga rimanente?</h1>';
        }
         else
        {
            res_msg = '<h1>Vuoi veramente spuntare le ' + dsp + ' righe rimanenti?</h1>';
        }
        /*--------------------------------*/
        /* Box di conferma operazione     */
        /*--------------------------------*/
        $('#dialog-confirm').html(res_msg);
        $('#dialog-confirm').dialog("option","title","ATTENZIONE !").dialog('open');
    });
    
/*----------------------------------------*/
/* Fine funzione livello '2'              */
/*========================================*/
}

/*========================================*/
/* Funzione di gestione livello '3'       */
/*                                        */
/* Input quantita' per ubicazione         */
/*----------------------------------------*/
function liv_003 ()
{
    /*====================================*/
    /* Focus su ubicazione                */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#cod_ubi').focus()                 ;
    $('#cod_ubi').select()                ;

    /*====================================*/
    /* Bottone fine modifica              */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#but_fip').click(function()
    {
        /*--------------------------------*/
        /* A livello precedente           */
        /*--------------------------------*/
        $('#liv_ope').val('2')            ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });

    /*====================================*/
    /* Bottone refresh                    */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });


    /*====================================*/
    /* Rilevazione tasto funzione Tangram */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('.numbersOnly').keyup(function(e)
    {
        /*--------------------------------*/
        /* Rilevazione tasto              */
        /*--------------------------------*/
        var tgk = (e.keyCode ? 
	            e.keyCode : 
	            e.which)               ;
        if(tgk == 13){$("#ods_001").submit()};
    });           


    /*====================================*/
    /* Modifica box di spunta o quantita' */
    /*                                    */
    /* (livello '3')                      */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
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
        var num_dst = $('#num_dst').val();
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
        par[2] = "num_dst"                ;
        val[2] = num_dst                  ;
        /*--------------------------------*/
        par[3] = "cod_ubi"                ;
        val[3] = cod_ubi                  ;
        /*--------------------------------*/
        par[4] = "num_pro"                ;
        val[4] = num_pro                  ;
        /*--------------------------------*/
        par[5] = "qta_ubi"                ;
        val[5] = qta_ubi                  ;
        /*--------------------------------*/
        par[6] = "flg_spn"                ;
        val[6] = flg_spn                  ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleodsw0",
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
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#ods_001").submit()    ;
            }
        });
    });          

    /*====================================*/
    /* Accettazione quantita' verificata: */
    /* scorciatoia per copiare quantita'  */
    /* ordinata                           */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WU"                ;
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
        var num_dst = $('#num_dst').val();
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
        if (num_cln == 9)
        {
        
        
            /*----------------------------*/
            /* Test se ubicazione         */
            /*----------------------------*/
            if (cod_ubi =='')
            {
                alert ("Selezionare una ubicazione");
                return;
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
        par[2] = "num_dst"                ;
        val[2] = num_dst                  ;
        /*--------------------------------*/
        par[3] = "cod_ubi"                ;
        val[3] = cod_ubi                  ;
        /*--------------------------------*/
        par[4] = "num_pro"                ;
        val[4] = num_pro                  ;
        /*--------------------------------*/
        par[5] = "qta_ubi"                ;
        val[5] = qta_sub                  ;
        /*--------------------------------*/
        par[6] = "flg_spn"                ;
        val[6] = flg_spn                  ;
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        prm = ass_prm(par, val)           ;
        
        
        console.log(prm);
        
        
        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleodsw0",
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
 
 
        
        
    //    console.log(data);
        
        
 
 
 
 
 
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#ods_001").submit()    ;
            }
        });
            
            
            
            
            
            
            
            
            
            
            
            
            
        }

    });



    /*====================================*/
    /* Selezione ubicazione               */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
 //    $('.sel_ubi').click(function()
 //    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
 //        var testo = $(this).text()        ;
        
 //        $('.rig_ubi').text(testo);
 //        $('#ubi').val(testo);
        
  //   });







    /*====================================*/
    /* Cambio ubicazione                  */
    /*                                    */
    /* Livello '3'                        */
    /*                                    */
    /* ___ MIGLIORARE ___                 */
    /*------------------------------------*/
    $('#cod_ubi').change(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var cod_ubi = $('#cod_ubi').val() ;
        var num_pro = $('#num_pro').val() ;
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm_01 = "cod_ubi=" + cod_ubi ;
        var prm_02 = "num_pro=" + num_pro ;
        var prm = prm_01 + "&" +
                  prm_02                  ;
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
                {$("#ods_001").submit()}
                 else if (res == 'KU')
                {
                    alert ('Ubicazione non in giacenza');
                    $('#cod_ubi').val('');
                }
                 else
                {
                    alert ('Ubicazione inesistente');
                    $('#cod_ubi').val('');
                }
            } 
        });    
    });          

/*----------------------------------------*/
/* Fine funzione livello '3'              */
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


