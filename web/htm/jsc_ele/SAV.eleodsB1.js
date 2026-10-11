/*========================================*/
/* eleodsB1 - Javascript - by WIP         */
/* -------------------------------------- */
/* Aggiornamento   : 21/05/2024           */
/*========================================*/

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
     else if (liv_ope == 3)
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
    /* Definizione box di dialogo per     */
    /* espansione giacenze per ubicazione */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal   : true,
        title   : 'Scheda prodotto',
        width   : 450,
        height  : 450,
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
        $('#prt_ods').val('')             ;
        $('#num_dst').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $("#ods_001").submit()            ;
    });

    /*====================================*/
    /* Focus su protocollo                */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#prt_ods').focus()                 ;
    $('#prt_ods').select()                ;
    cel_css("cel_num", "+")               ;

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

    /*====================================*/
    /* Eventuale espansione riga lista    */
    /* documenti per elenco responsabili  */
    /*                                    */
    /* Livello '1'                        */
    /*------------------------------------*/
    $('#lst td').click(function()
    {
        /*================================*/
        /* Numero colonna                 */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));

        /*================================*/
        /* Se colonna del responsabile    */
        /*--------------------------------*/
        if (num_cln == 5)
        {
            /*----------------------------*/
            /* Identificatore documento   */
            /*----------------------------*/
            var prt_lst = $(this).closest("tr").find('#prt_lst').val();
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./eleodsr0",
                contentType: "application/x-www-form-urlencoded",
                data: "prt_lst=" + prt_lst,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog("option","title","Responsabili documento").dialog('open');
                }
            });
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
    /* Definizione box per messaggi       */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $("#dlg_ms2").dialog({
        modal    : true,
        bgiframe : true,
        height   : "auto",
        width    : 400,
        autoOpen : false,
        closeOnEscape: false,
        resizable: false,
        title    : '*** ATTENZIONE ***'
    });

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione giacenze per ubicazione */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        modal   : true,
        title   : 'Scheda prodotto',
        width   : 450,
        height  : 450,
    });

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#dialog-confirm').dialog({
      autoOpen: false,
      resizable: false,
      height: "auto",
      width: 400,
      modal: true,
      closeOnEscape: false,
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
        /*--------------------------------*/
      open: function()
      {
          $(this).siblings('.ui-dialog-buttonpane').find('button:eq(1)').focus(); 
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
    /* Bottone stampa etichette Zebra     */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#but_zbr').click(function()
    {
        /*--------------------------------*/
        /* Identificatori riga tabella    */
        /*--------------------------------*/
        var ide_row = "#row_001"          ;
        /*--------------------------------*/
        /* Valori da testata tabella      */
        /*--------------------------------*/
        var rag_cli = $('#rag_cli').val() ;
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var prt_ods = $(ide_row).find('#prt').val();
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "prt_ods": prt_ods,
            "rag_cli": rag_cli
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;
        /*--------------------------------*/
        /* Messaggio per la richiesta di  */
        /* conferma                       */
        /*--------------------------------*/
        var cnf_msg = "CARPENTERIA PRONTA - STAMPA ETICHETTA ?";
        /*================================*/
        /* Richiesta di conferma spunta   */
        /*--------------------------------*/
        cnf_dlg(cnf_msg).done(function()
        {
        /*================================*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
      //    url: "./eleodsZ0.pl",
              url: "./eleodsZ0",
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
                $("#ods_001").submit()    ;
            }
        });

        /*================================*/
        /* Scelta 'NO'                    */
        /*--------------------------------*/
        }).fail(function()
        {
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#ods_001").submit()    ;
        });
    });          

    /*====================================*/
    /* Colorazione di rosso ubicazione    */
    /* principale                         */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('#rig tr').each(function()
    {
        /*--------------------------------*/
        /* Estrazione ubicazione princi-  */
        /* pale                           */
        /*--------------------------------*/
        var pri_ubi = $(this).find('#ubp').val();
        /*--------------------------------*/
        /* Estrazione literal ubicazione  */
        /*--------------------------------*/
        var lit_ubi = $(this).find('#lit_ubi').text();
        /*--------------------------------*/
        /* Pulizia literal ubicazione     */
        /*--------------------------------*/
        lit_ubi = lit_ubi.trim()          ;
        /*--------------------------------*/
        /* Pulizia literal ubicazione     */
        /*--------------------------------*/
        if (lit_ubi !== '')
        {
            /*----------------------------*/
            /* Individuazione eventuale   */
            /* ubicazione principale      */
            /*----------------------------*/
            if (lit_ubi.includes(pri_ubi)) 
            {
                /*------------------------*/
                /* Sostituzione ubicazione*/
                /* principale             */
                /*------------------------*/
                var inx_wrd = lit_ubi.replace(pri_ubi, `<span style="color: red;">${pri_ubi}</span>`);
                lit_ubi = inx_wrd         ;
                /*------------------------*/
                /* Scrittura nuova        */
                /* stringa con ubicazioni */
                /* in grassetto           */
                /*------------------------*/
                $(this).find('#lit_ubi').html(lit_ubi);
                $(this).find('#lit_ubi').css('font-weight', 'bold');
            }
        } 
        else 
        {
            /*----------------------------*/
            /* Stringa vuota              */
            /*----------------------------*/
            $(this).find('#lit_ubi').html('');
        }
    });

    /*====================================*/
    /* Inibizione totale se semaforo      */
    /* verde                              */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    var per_sem = $('#cel_sem').find('img').attr('src');
    var fil_nam = per_sem.split('/').pop();
    if (fil_nam == 'green.png')
    {
        $('input[type=checkbox]').attr('disabled','true');
    }

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
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_dst = $("#num_dst").val() ;
        var alf_pro = $('#alf_pro').val() ;
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            "num_dst": num_dst,
            "alf_pro": alf_pro
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);
        /*--------------------------------*/
        /* Lettura                        */
        /*--------------------------------*/
        $.ajax({ type: 'POST',
                  url: "./eleodsp0",
          contentType: "application/x-www-form-urlencoded",
                 data: dat,
              success: function(data)
              {
                /*------------------------*/
                /* Esito lettura          */
                /*------------------------*/
                let res = data.substring(0, 2);
                /*------------------------*/
                /* Se test superato       */
                /*------------------------*/
                if (res == 'OK')
                {
                    /*--------------------*/
                    /* Livello successivo */
                    /*--------------------*/
                    $('#liv_ope').val('3');
                }
                /*------------------------*/
                /* Caricamento del modulo */
                /*------------------------*/
                $("#ods_001").submit()    ;
            } 
        });    
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
        
//        alert(num_cln);
        
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
        /* Se colonna codice prodotto     */
        /*--------------------------------*/
        if (num_cln == 11)
        {
            /*----------------------------*/
            /* A livello successivo solo  */
            /* se NON spuntata            */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                /*------------------------*/
                /* Codice prodotto        */
                /*------------------------*/
                $('#alf_pro').val(alf_pro);
                /*------------------------*/
                /* A livello successivo   */
                /*------------------------*/
                $('#liv_ope').val('3')    ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#ods_001").submit()    ;
            }
        }

        /*================================*/
        /* Se colonna quantita' ordinata  */
        /*--------------------------------*/
        if (num_cln == 15)
        {
            /*----------------------------*/
            /* Test se spunta effettuata  */
            /*----------------------------*/
            if (flg_spn == 'N')
            {
                /*------------------------*/
                /* Copia della quantita'  */
                /*------------------------*/
                var qta_sub = $(this).closest("tr").find('#qta').val();
                $(this).closest("tr").find('#qtv').val(qta_sub);
            }
        }

        /*================================*/
        /* Se colonna dell'ubicazione     */
        /*--------------------------------*/
        if (num_cln == 18)
        {
            /*----------------------------*/
            /* Codice prodotto            */
            /*----------------------------*/
            var alf_pro = $(this).closest("tr").find('#alf').val();
            /*----------------------------*/
            /* Riempimento box            */
            /*----------------------------*/
            $.ajax({
                type: "POST",
                url: "./eleodsg0",
                contentType: "application/x-www-form-urlencoded",
                data: "alf_pro=" + alf_pro,
                success: function (data)
                {
                      $("#dialog").html(data);
                      $("#dialog").dialog("option","title","Prodotto : " + alf_pro).dialog('open');
                }
            });
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
        if(tgk == 13){$("#ods_001").submit()};
    });           

    /*====================================*/
    /* Modifica quantita'                 */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
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
        /* Messaggio di quantita' su-     */
        /* periore a quella in ordine     */
        /*--------------------------------*/
        if (qtv_ods > qta_ods)
        {
            /*----------------------------*/
            /* Normalizzazione            */
            /*----------------------------*/
            $(ide_row).find('#qtv').val(0);
            qtv_ods = 0                   ;
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            alert("Quantita' non accettabile!");
            /*----------------------------*/
            /* Uscita                     */
            /*----------------------------*/
            return                        ;
        }

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm =
        {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "prt_ods": prt_ods,
            "prg_ods": prg_ods,
            "pri_ods": pri_ods,
            "qta_ods": qta_ods,
            "qtv_ods": qtv_ods,
            "flg_spn": flg_spn
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

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
            url: "./eleodsx0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
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
    /* Modifica box di spunta             */
    /*                                    */
    /* Livello '2'                        */
    /*------------------------------------*/
    $('input[type="checkbox"]').change(function()
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
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "prt_ods": prt_ods,
            "prg_ods": prg_ods,
            "pri_ods": pri_ods,
            "qta_ods": qta_ods,
            "qtv_ods": qtv_ods,
            "flg_spn": flg_spn
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm)          ;

        /*--------------------------------*/
        /* Messaggio per la richiesta di  */
        /* conferma                       */
        /*--------------------------------*/
        var cnf_msg = ''                  ;
        if (flg_spn == 'N')
        {
            cnf_msg = 'Vuoi togliere la spunta al prodotto?'
        }
         else
        {
            cnf_msg = 'Vuoi spuntare manualmente il prodotto?'
        }

        /*================================*/
        /* Richiesta di conferma spunta   */
        /*--------------------------------*/
        cnf_dlg(cnf_msg).done(function()
        {
        /*================================*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleodsx0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Focus su prodotto      */
                /*------------------------*/
                $('#alf_pro').focus()     ;
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#ods_001").submit()    ;
            }
        });
        /*================================*/
        /* Scelta 'NO'                    */
        /*--------------------------------*/
        }).fail(function()
        {
            /*----------------------------*/
            /* Annullamento spunta        */
            /*----------------------------*/
            $(this).prop('checked',false);
            /*----------------------------*/
            /* Ricarica modulo            */
            /*----------------------------*/
            $("#ods_001").submit()        ;
        });
    });          

    /*====================================*/
    /* Definizione box di dialogo -       */
    /* opzioni                            */
    /*                                    */
    /* Livello '2'                        */
    /*                                    */
    /* ATTUALMENTE DISATTIVATA            */
    /*------------------------------------*/
    $('#dialog-confirm').dialog('option', 'buttons',
    {
        /*--------------------------------*/
        /* Scelta 'SI'                    */
        /*--------------------------------*/
        "si" : function()
        {
            /*----------------------------*/
            /* Box di dialogo: close      */
            /*----------------------------*/
            $(this).dialog("close")       ;
            

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
        },
        
        
        
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
        /* ATTUALMENTE INIBITO PERCHE'    */
        /* BISOGNA DECIDERE LE UBICAZIONI */
        /* DI PRELIEVO                    */
        /* ___                            */
        /*--------------------------------*/
        $('#dlg_ms2').html('<H1>FUNZIONE DISATTIVATA !</H1>');
        $('#dlg_ms2').dialog("open");                
        
    //    alert ("FUNZIONE DISATTIVATA!")   ;
        return                            ;
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
    /* Definizione box per messaggi       */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $("#dlg_ms3").dialog({
        modal    : true,
        bgiframe : true,
        height   : "auto",
        width    : 400,
        autoOpen : false,
        closeOnEscape: false,
        resizable: false,
        title    : '*** ATTENZIONE ***'
    });

    /*====================================*/
    /* Focus su ubicazione e quantita'    */
    /*                                    */
    /* Livello '3'                        */
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
    /* Highlight quantita'                */
    /*                                    */
    /* Livello '3'                        */
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
    /* Modifica quantita'                 */
    /*                                    */
    /* (livello '3')                      */
    /*------------------------------------*/
    $('.numbersOnly').change(function()
    {
        /*--------------------------------*/
        /* Codice ubicazione              */
        /*--------------------------------*/
        var cod_ubi = $(this).closest("tr").find('#ubi').val();
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
            /*----------------------------*/
            /* Normalizzazione quantita'  */
            /*----------------------------*/
            $(this).closest("tr").find('#qtv').val('');
//            $('#qtv').val('')             ;
            $('#cod_ubi').focus()         ;
            $('#cod_ubi').select()        ;
            /*----------------------------*/
            /* Uscita                     */
            /*----------------------------*/
            return                        ;
        }
    
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
        var qta_ord = $(ide_row).find('#qta').val();
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
        /* Messaggio di quantita' su-     */
        /* periore a quella in ordine     */
        /*--------------------------------*/
        var dif = qta_ord - qta_ubi       ;
        if (dif < 0)
        {
            /*----------------------------*/
            /* Normalizzazione            */
            /*----------------------------*/
            $(ide_row).find('#qtv').val(0);
            qta_ubi = 0                   ;
            /*----------------------------*/
            /* Messaggio                  */
            /*----------------------------*/
            alert("Quantita' non accettabile!");
            /*----------------------------*/
            /* Uscita                     */
            /*----------------------------*/
            return                        ;
        }

        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "num_dst": num_dst,
            "cod_ubi": cod_ubi,
            "num_pro": num_pro,
            "qta_ubi": qta_ubi,
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
            url: "./eleodsx0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
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
                return                    ;
            }
        
            /*----------------------------*/
            /* Copia della quantita'      */
            /*----------------------------*/
            var qta_sub = $(this).closest("tr").find('#qta').val();
            $(this).closest("tr").find('#qtv').val(qta_sub);
            
        /*--------------------------------*/
        /* Oggetto con i Parametri        */
        /*--------------------------------*/
        const prm = {
            "rsp_doc": rsp_doc,
            "tip_ope": tip_ope,
            "num_dst": num_dst,
            "cod_ubi": cod_ubi,
            "num_pro": num_pro,
            "qta_ubi": qta_sub,
            "flg_spn": flg_spn
        }
        /*--------------------------------*/
        /* Assemblaggio parametri         */
        /*--------------------------------*/
        const dat = $.param(prm);

        /*--------------------------------*/
        /* Aggiornamento Tangram          */
        /*--------------------------------*/
        $.ajax({
            /*----------------------------*/
            /* Richiamo modulo            */
            /*----------------------------*/
            type: "POST",
            url: "./eleodsx0",
            contentType: "application/x-www-form-urlencoded",
            data: dat,
            /*----------------------------*/
            /* Se andato a buon fine      */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $("#ods_001").submit()    ;
            }
        });
            
        }

    });

    /*====================================*/
    /* Controllo inserimento ubicazione   */
    /*                                    */
    /* Livello '3'                        */
    /*------------------------------------*/
    $('#cod_ubi').keydown(function(e)
    {
        /*--------------------------------*/
        /* Controllo se tasto enter       */
        /*--------------------------------*/
        if (e.keyCode === 13)
        {
            /*----------------------------*/
            /* Controllo tasto            */
            /*----------------------------*/
            e.preventDefault()            ;
            /*----------------------------*/
            /* Valori da riga tabella     */
            /*----------------------------*/
            var cod_ubi = $('#cod_ubi').val();
            var num_pro = $('#num_pro').val();
            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "cod_ubi": cod_ubi,
                "num_pro": num_pro
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;

            /*----------------------------*/
            /* Lettura                    */
            /*----------------------------*/
            $.ajax({ type: 'POST',
                      url: "./eleodsu0",
              contentType: "application/x-www-form-urlencoded",
                     data: dat,
                  success: function(data)
                {
                /*------------------------*/
                /* Valore in output       */
                /*------------------------*/
                let res = data.substring(0, 2);

                /*========================*/
                /* Se 'OK'                */
                /*------------------------*/
                if (res == 'OK')
                {
                    /*--------------------*/
                    /* Esecuzione modulo  */
                    /*--------------------*/
                    $("#ods_001").submit();
                }
                /*========================*/
                /* Se non in giacenza     */
                /*------------------------*/
                else if (res == 'KU')
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    alert('Prodotto non in ubicazione!');
                    /*--------------------*/
                    /* Normalizzazione    */
                    /* Ubicazione         */
                    /*--------------------*/
                    $('#cod_ubi').val('');
                }
                /*========================*/
                /* Se inesistente         */
                /*------------------------*/
                 else
                {
                    /*--------------------*/
                    /* Messaggio          */
                    /*--------------------*/
                    alert('Ubicazione inesistente');
                    /*--------------------*/
                    /* Normalizzazione    */
                    /* Ubicazione         */
                    /*--------------------*/
                    $('#cod_ubi').val('') ;                
                }
            } 
            /*----------------------------*/
            /* Lettura                    */
            /*                            */
            /* Fine                       */
            /*----------------------------*/
            });
        /*--------------------------------*/
        /* Controllo se tasto enter       */
        /*                                */
        /* Fine                           */
        /*--------------------------------*/
        }
    /*------------------------------------*/
    /* Controllo inserimento ubicazione   */
    /*                                    */
    /* Livello '3'                        */
    /*                                    */
    /* Fine                               */
    /*====================================*/
    });

/*----------------------------------------*/
/* Fine funzione livello '3'              */
/*========================================*/
}

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
/* Funzione di richiesta conferma spunta  */
/*----------------------------------------*/
function cnf_dlg(msg)
{
    /*------------------------------------*/
    /* Messaggio per il box               */
    /*------------------------------------*/
    var dialog = $("<div>"+msg+"</div>")  ;
    /*------------------------------------*/
    /* Variabile per si/no                */
    /*------------------------------------*/
    var def = $.Deferred()                ;
    /*------------------------------------*/
    /* Creazione finestra di dialogo      */
    /*------------------------------------*/
    $(dialog).dialog(
    {
        /*--------------------------------*/
        /* Parametri box                  */
        /*--------------------------------*/
        resizable: false,
        autoOpen : true,
        title    : "ATTENZIONE !",
        height   : "auto",
        width    : 600,
        modal    : true,
        buttons:
        {
            'si': function()
            {
                def.resolve();
                $(dialog).dialog( "close" );
            },
            'NO': function()
            {
                def.reject();
                $(dialog).dialog( "close" );
            }
        },
        /*--------------------------------*/
        /* Bottone di default (NO)        */
        /*--------------------------------*/
        open: function()
        {
            $(this).siblings('.ui-dialog-buttonpane').find('button:eq(1)').focus(); 
        }
        
    });
    /*------------------------------------*/
    /* Variabile in uscita                */
    /*------------------------------------*/
    return def.promise()                  ;
/*----------------------------------------*/
/* Fine funzione                          */
/*========================================*/
}
