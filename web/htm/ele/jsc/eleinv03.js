/*========================================*/
/* eleinv03 - Javascript - by WIP         */
/*                                        */
/* Elettra : Inventario per Prodotto      */
/* -------------------------------------- */
/* Aggiornamento   : 13/11/2024           */
/*========================================*/

/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#inv_003"                      ;
url_wrt = "../ele/eleinvw0"               ;
url_ubi = "../ele/eleinvu0"               ;
url_rsp = "../getip.pl"                   ;

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{



    /*====================================*/
    /* INIBIZIONE TOTALE PROVVISORIA      */
    /*------------------------------------*/
//    $(":input[type=text]").prop("disabled", true);
    $("#cod_ubi").prop("disabled", true);
    $(".ncf").prop("disabled", true);
    $(":input[type=number]").prop("disabled", true);
    $(":input[type=checkbox]").prop("disabled", true);




    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione elenco 'check'          */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal: true,
        title: 'Scheda prodotto',
        width: 950,
        height: 300,
    });

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*------------------------------------*/
    $('#dialog-confirm').dialog({
      autoOpen: false,
      resizable: false,
      height: "auto",
      width: 400,
      modal: true
    });    

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*                                    */
    /* Default 'NO'                       */
    /*------------------------------------*/
    $("#dialog-confirm").dialog({
        open: function()
        {
         $(this).siblings('.ui-dialog-buttonpane').find('button:eq(1)').focus(); 
        }
    });

    /*====================================*/
    /* Definizione box di dialogo -       */
    /* opzioni                            */
    /*------------------------------------*/
    $('#dialog-confirm').dialog('option', 'buttons',
    {
        /*--------------------------------*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        "si" : function()
        {
            /*----------------------------*/
            /* Tipo operazione            */
            /*----------------------------*/
            var tip_ope = "WD"            ;
            /*----------------------------*/
            /* Identificatori riga        */
            /*----------------------------*/
            var nrr = $('#dialog-confirm').data('riga');
            var ide_row = "#row_" + nrr   ;
            /*----------------------------*/
            /* Valori da riga tabella     */
            /*----------------------------*/
            var cod_ubi = $(ide_row).find('#ubi').val();
            var alf_pro = $(ide_row).find('#alf').val();
            var num_pro = $(ide_row).find('#pro').val();
            
            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "tip_ope": tip_ope,
                "cod_ubi": cod_ubi,
                "alf_pro": alf_pro,
                "num_pro": num_pro
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;
            
            /*----------------------------*/
            /* Aggiornamento Tangram      */
            /*----------------------------*/
            $.ajax({
                /*----------------------------*/
                /* Richiamo modulo            */
                /*----------------------------*/
                type: "POST",
                url: url_wrt,
                contentType: "application/x-www-form-urlencoded",
                data: dat,
                /*----------------------------*/
                /* Se andato a buon fine      */
                /*----------------------------*/
                success: function (data)
                {
                    /*------------------------*/
                    /* Caricamento del modulo */
                    /*------------------------*/
                    $(mod_frm).submit()       ;
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
    /* Responsabile                       */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_des').val()         ;

    /*====================================*/
    /* Codice prodotto                    */
    /*------------------------------------*/
    alf_pro = $('#alf_pro').val()         ;

    /*====================================*/
    /* Test su presenza responsabile      */
    /*------------------------------------*/
    if (!rsp_doc || rsp_doc == 0)
    {
        /*--------------------------------*/
        /* Disabilitazione prodotto       */
        /*--------------------------------*/
        $('#alf_pro').prop("disabled",true);
        /*--------------------------------*/
        /* Disabilitazione ubicazioni     */
        /*--------------------------------*/
        $('#acc_ub1').prop("disabled",true);
        $('#acc_ub2').prop("disabled",true);
        $('#acc_ub3').prop("disabled",true);
        $('#acc_ub4').prop("disabled",true);
        /*--------------------------------*/
        /* Focus su responsabile          */
        /*--------------------------------*/
        $('#rsp_doc').focus()             ;
        /*--------------------------------*/
        /* Evidenziazione celle           */
        /*--------------------------------*/
        $("#tes td").css({"background-color": "#BBB"});
        /*--------------------------------*/
        /* Cella responsabile in rosso    */
        /*--------------------------------*/
        cel_css("cel_ute", "R")           ;
        cel_css("cel_pro", "-")           ;
        /*--------------------------------*/
        /* Test iniziale su indirizzo IP  */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url : url_rsp,
            contentType: "application/x-www-form-urlencoded",
            /*--------------------------------*/
            /* Se indirizzo determinato       */
            /*--------------------------------*/
            success: function (data)
            {
                /*----------------------------*/
                /* Determinazione operatore   */
                /*----------------------------*/
                var ipx_val = data.trim()     ;
                let_ipx(ipx_val)              ;
            }
        });
    }
     else
    {
        /*--------------------------------*/
        /* Abilitazione prodotto          */
        /*--------------------------------*/
        $('#alf_pro').prop("enabled",true);
        $("#tes td").css({"background-color": "#BBB"});
        cel_css("cel_ute", "-")           ;
        cel_css("cel_pro", "+")           ;
        $('#alf_pro').focus()             ;
        $('#alf_pro').select()            ;
    }

    /*====================================*/
    /* Test su presenza codice prodotto   */
    /*------------------------------------*/
    if (!alf_pro || alf_pro == 0)
    {
        /*--------------------------------*/
        /* Disabilitazione ubicazioni     */
        /*--------------------------------*/
        $('#acc_ub1').prop("disabled",true);
        $('#acc_ub2').prop("disabled",true);
        $('#acc_ub3').prop("disabled",true);
        $('#acc_ub4').prop("disabled",true);
    }
     else
    {
        /*--------------------------------*/
        /* Abilitazione ubicazioni        */
        /*--------------------------------*/
        $('#acc_ub1').prop("enabled",true);
        $('#acc_ub2').prop("enabled",true);
        $('#acc_ub3').prop("enabled",true);
        $('#acc_ub4').prop("enabled",true);
        /*--------------------------------*/
        /* Cella responsabile in verde    */
        /*--------------------------------*/
        cel_css("cel_ute", "G")           ;
    }

    /*====================================*/
    /* Cambio Operatore                   */
    /*------------------------------------*/
    $('#rsp_doc').change(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Selezionato prodotto               */
    /*------------------------------------*/
    $('#alf_pro').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
    });          

    /*====================================*/
    /* Cambio prodotto                    */
    /*------------------------------------*/
    $('#alf_pro').change(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Highlight quantita'                */
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
    /* Modifica ubicazione in chiave      */
    /*------------------------------------*/
    $('#cod_ubi').change(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var cod_ubi = $('#cod_ubi').val() ;
            cod_ubi = cod_ubi.trim()      ;
            cod_ubi = pul_str (cod_ubi)   ;
            cod_ubi = cod_ubi.toUpperCase();
        $('#cod_ubi').val(cod_ubi)        ;
        /*--------------------------------*/
        /* Scansione tabella per duplica- */
        /* ti ubicazione eventuali        */
        /*--------------------------------*/
        $("#rig tr").each(function()
        {
            /*----------------------------*/
            /* Codice per il confronto    */
            /*----------------------------*/
            var bcd_txt = $(this).find('#ubi').val();
            /*----------------------------*/
            /* Carattere accentato per il */
            /* messaggio a video          */
            /*----------------------------*/
            var aaa_acc = '\u00E0'        ;
            /*----------------------------*/
            /* Se ubicazione presente     */
            /*----------------------------*/
            if (bcd_txt && bcd_txt === cod_ubi)
            {
                /*------------------------*/
                /* Messaggio              */
                /*------------------------*/
                alert('Ubicazione gi' + aaa_acc + ' presente in tabella');
                return false;
            }            
        });                  
    
        /*--------------------------------*/
        /* Parametro per lettura          */
        /*--------------------------------*/
        var prm = 'cod_ubi=' + cod_ubi    ;
        /*--------------------------------*/
        /* Lettura                        */
        /*--------------------------------*/
        $.ajax({ type: 'POST',
                  url: url_ubi,
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
                if (res != 'OK')
                {alert ('Ubicazione inesistente');}
            } 
        });    
    });          

    /*====================================*/
    /* Bottone nuovo record               */
    /*------------------------------------*/
    $('#new').click(function()
    {
        /*--------------------------------*/
        /* Codice ubicazione              */
        /*--------------------------------*/
        var cod_ubi = $('#cod_ubi').val();

        if (cod_ubi == '')
        {
            alert('Inserire un codice ubicazione');
            return                       ;
        };

        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WR"                ;
        /*--------------------------------*/
        /* Valori da tabella              */
        /*--------------------------------*/
        var num_pro = $('#num_pro').val();
        
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "tip_ope": tip_ope,
            "num_pro": num_pro,
            "cod_ubi": cod_ubi
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url : url_wrt,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        });

    });

    /*====================================*/
    /* Bottone Non conformita'            */
    /*------------------------------------*/
    $('.ncf').click(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_rw2 = "#rw2_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Apertura / chiusura riga       */
        /*--------------------------------*/
        $(ide_rw2).toggle()               ;
        /*--------------------------------*/
        /* Focus                          */
        /*--------------------------------*/
        $(this).closest("tr").find('#nct').focus();
    });

    /*====================================*/
    /* Bottone Cancella                   */
    /*------------------------------------*/
    $('.del_btn').click(function()
    {
        /*--------------------------------*/
        /* Messaggio conferma operazione  */
        /*--------------------------------*/
        var res_msg = 'Vuoi veramente cancellare questa ubicazione?';
        /*--------------------------------*/
        /* Box di conferma operazione     */
        /*--------------------------------*/
        var riga = $(this).closest("tr").find('#pri').val();
        var my_d = $("#dialog-confirm").data('riga', riga);
        $('#dialog-confirm').html(res_msg);
        $('#dialog-confirm').dialog("option","title","ATTENZIONE !").dialog('open');
    });

    /*====================================*/
    /* Accettazione testo Non conformita' */
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
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#rw2_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var alf_pro = $(ide_row).find('#alf').val();
        var num_pro = $(ide_row).find('#pro').val();
        var qta_prs = $(ide_row).find('#qta').val();
        var qta_rlv = $(ide_row).find('#qtv').val();
        var ann_ncf = $(ide_row).find('#nct').val();
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
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "cod_ubi": cod_ubi,
            "alf_pro": alf_pro,
            "num_pro": num_pro,
            "qta_prs": qta_prs,
            "qta_rlv": qta_rlv,
            "flg_spn": flg_spn,
            "ann_ncf": ann_ncf
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url : url_wrt,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        });
    });          


    /*====================================*/
    /* Modifica ubicazione in riga        */
    /*------------------------------------*/
    $('.acc_ubi').change(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var ubi = $(ide_row).find('#ubi').val();
        var prm = 'cod_ubi=' + ubi        ;
        /*--------------------------------*/
        /* Lettura                        */
        /*--------------------------------*/
        $.ajax({ type: 'POST',
                  url: url_ubi,
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
                if (res != 'OK')
                {alert ('Ubicazione inesistente');}
            } 
        });    
    });          

    /*====================================*/
    /* Modifica box di spunta, quantita'  */
    /* o ubicazione                       */
    /*------------------------------------*/
    $('input[type="checkbox"],.numbersOnly,.acc_ubi').change(function()
    {
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "WI"                ;
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var rsp_doc = $('#rsp_doc').val();
        var cod_ubi = $(ide_row).find('#ubi').val();
        var alf_pro = $(ide_row).find('#alf').val();
        var num_pro = $(ide_row).find('#pro').val();
        var qta_prs = $(ide_row).find('#qta').val();
        var qta_rlv = $(ide_row).find('#qtv').val();
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
        if (qta_rlv == 0 && flg_spn == 'S')
           {alert("Quantita' a zero!");}  ;

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "cod_ubi": cod_ubi,
            "alf_pro": alf_pro,
            "num_pro": num_pro,
            "qta_prs": qta_prs,
            "qta_rlv": qta_rlv,
            "flg_spn": flg_spn
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url : url_wrt,
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $(mod_frm).submit()       ;
            }
        });
    });          

    /*====================================*/
    /* Controllo click su elementi riga   */
    /*------------------------------------*/
    $('#rig td').click(function()
    {
        /*--------------------------------*/
        /* Indicatori colonne             */
        /*--------------------------------*/
        var cln_qta = 11                  ;
        /*--------------------------------*/
        /* Identificatore riga tabella    */
        /*--------------------------------*/
        var pri_inv = $(this).closest("tr").find('#pri').val();
        var ide_row = "#row_" +
                      pri_inv             ;
                      
        /*--------------------------------*/
        /* Pulizia highlight campo quan-  */
        /* tita' di cuascuna riga         */
        /*--------------------------------*/
        $("#rig tr").each(function()
        {
            $(this).closest("tr").css("background-color","#FFF");
        });                  
        /*--------------------------------*/
        /* Highlight campo quantita'      */
        /*--------------------------------*/
        $(this).closest("tr").css("background-color","#CCC");
                      
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
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

        /*================================*/
        /* Test che sia la colonna della  */
        /* quantita' ordinata e non ci    */
        /* sia gia' la spunta             */
        /*--------------------------------*/
        if (num_cln == cln_qta)
        {
            /*----------------------------*/
            /* Test se riga spuntata      */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                /*------------------------*/
                /* Copia della quantita'  */
                /*------------------------*/
                var qta = $(this).closest("tr").find('#qtp').val();
                $(this).closest("tr").find('#qtv').val(qta);
            }
        }
        
    /*------------------------------------*/
    /* Controllo click su elementi riga   */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

    /*====================================*/
    /* Bottone reset                      */
    /*------------------------------------*/
    $("#but_res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $(':input',mod_frm)
          .not(':button, :submit, :reset, :hidden')
          .val('')
          .prop('checked', false)
          .prop('selected', false)        ;     
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
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

/*========================================*/
/* Lettura nodo IP per determinare il     */
/* codice operatore                       */
/*----------------------------------------*/
function let_ipx(val_ipx)
{
    /*------------------------------------*/
    /* Normalizzazione nodo IP            */
    /*------------------------------------*/
    var ipx_arr = val_ipx.split(".")      ;
    var ipw = ipx_arr[3]                  ;
    var ipw = pad(ipw, 3)                 ;
    /*------------------------------------*/
    /* Oggetto con i Parametri            */
    /*------------------------------------*/
    const prm =
    {
        "val_ipx": ipw
    }
    /*------------------------------------*/
    /* Assemblaggio parametri             */
    /*------------------------------------*/
    const dat = $.param(prm)              ;
    /*------------------------------------*/
    /* Lettura                            */
    /*------------------------------------*/
    $.ajax({ type: 'POST',
              url: "../elebfoi0",
      contentType: "application/x-www-form-urlencoded",
             data: dat,
          success: function(data)
          {
              /*--------------------------*/
              /* Esito lettura            */
              /*--------------------------*/
              let res = data.substring(0, 3);
                  res = res.trim()        ;
                  res = parseInt(res)     ;
                  $('#rsp_doc').val(res)  ;

              /*--------------------------*/
              /* Test su esito lettura    */
              /*--------------------------*/
              if (res == 0 || !res)
              {
                  /*----------------------*/
                  /* Se non trovato: nor- */
                  /* malizzazione codice  */
                  /* operatore            */
                  /*----------------------*/
                  $('#rsp_doc').val(0)    ;
                  /*----------------------*/
                  /* Uscita               */
                  /*----------------------*/
                    return                ;
              }
               else
              {
                  /*----------------------*/
                  /* Operatore determina- */
                  /* to                   */
                  /*----------------------*/
                  $('#rsp_doc').val(res)  ;
                  /*----------------------*/
                  /* A livello successivo */
                  /*----------------------*/
                  $('#liv_ope').val('1')  ;
                  $(mod_frm).submit()     ;
              }
          }
    });
/*----------------------------------------*/
/* Lettura nodo IP - fine                 */
/*========================================*/
}

/*========================================*/
/* Formattazione numeri con zeri in testa */
/*----------------------------------------*/
function pad (str, max)
{
    str = str.toString()                  ;
    return str.length < max ? pad("0" + str, max) : str;
}

/*========================================*/
/* Funzione di pulizia stringa            */
/*                                        */
/* N.B.: usata per togliere caratteri non */
/*       alfabetici o numerici            */
/*----------------------------------------*/
function pul_str(inp_str)
{
    return inp_str.replace(/[^a-zA-Z0-9]/g, '');
};    
