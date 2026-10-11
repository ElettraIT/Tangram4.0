/*========================================*/
/* eleubi01 - Javascript - N&A            */
/* -------------------------------------- */
/* Aggiornamento   : 17/01/2025           */
/*========================================*/
/*========================================*/
/* Variabili statiche                     */
/*----------------------------------------*/
mod_frm = "#ubi_001"                      ;

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Campi form                         */
    /*------------------------------------*/
    rsp_doc = $('#rsp_doc').val()         ;
    rsp_des = $('#rsp_des').val()         ;
    num_pro = $('#num_pro').val()         ;
    alf_pro = $('#alf_pro').val()         ;
    ubi_qta = $('#ubi_qta').val()         ;
    
    acc_ub1 = $('#acc_ub1').val()         ;
    acc_ub1 = acc_ub1.trim()              ;
    acc_ub1 = pul_str (acc_ub1)           ;
    
    acc_ub2 = $('#acc_ub2').val()         ;
    acc_ub2 = acc_ub2.trim()              ;
    acc_ub2 = pul_str (acc_ub2)           ;

    /*====================================*/
    /* Bottone reset                      */
    /*------------------------------------*/
    $("#res").click(function()
    {
        /*--------------------------------*/
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#alf_pro').val('')             ;
        $('#ubi_qta').val('')             ;
        $('#acc_ub1').val('')             ;
        $('#acc_ub2').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* espansione giacenze per ubicazione */
    /*                                    */
    /*------------------------------------*/
    $("#dialog").dialog({
        autoOpen: false,
        title   : 'Scheda prodotto',
        width   : 600,
        height  : 450,
        modal   : true
    });

    /*====================================*/
    /* Definizione box di dialogo per     */
    /* conferma spunta totale             */
    /*------------------------------------*/
    $('#dialog-confirm').dialog({
      autoOpen: false,
      resizable: false,
      width: 400,
      height: "auto",
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
            var tip_ope = "CU"            ;
            /*----------------------------*/
            /* Oggetto con i Parametri    */
            /*----------------------------*/
            const prm =
            {
                "tip_ope": tip_ope,
                "rsp_doc": rsp_doc,
                "num_pro": num_pro,
                "ubi_qta": ubi_qta,
                "acc_ub1": acc_ub1,
                "acc_ub2": acc_ub2
            }
            /*----------------------------*/
            /* Assemblaggio parametri     */
            /*----------------------------*/
            const dat = $.param(prm)      ;
            /*----------------------------*/
            /* Aggiornamento Tangram      */
            /*----------------------------*/
            $.ajax({
                /*------------------------*/
                /* Richiamo modulo        */
                /*------------------------*/
                type: "POST",
                url: "./eleubix0",
                contentType: "application/x-www-form-urlencoded",
                data: dat,
                /*------------------------*/
                /* Se andato a buon fine  */
                /*------------------------*/
                success: function (data)
                {
                    /*--------------------*/
                    /* Normalizzazioni    */
                    /*--------------------*/
                    $('#alf_pro').val('') ;
                    $('#ubi_qta').val('') ;
                    $('#acc_ub1').val('') ;
                    $('#acc_ub2').val('') ;
                    /*--------------------*/
                    /* Caricamento modulo */
                    /*--------------------*/
                    $(mod_frm).submit()   ;
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
    /* Test su presenza responsabile      */
    /*------------------------------------*/
    if (!rsp_doc || rsp_doc == 0)
    {
        /*--------------------------------*/
        /* Disabilitazione prodotto       */
        /*--------------------------------*/
        $('#alf_pro').prop("disabled",true);
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
        /* individuare l'operatore        */
        /*--------------------------------*/
        $.ajax({
            type: "POST",
            url : "./getip.pl",
            contentType: "application/x-www-form-urlencoded",
            /*----------------------------*/
            /* Se indirizzo determinato   */
            /*----------------------------*/
            success: function (data)
            {
                /*------------------------*/
                /* Determinazione         */
                /*------------------------*/
                var ipx_val = data.trim() ;
                let_ipx(ipx_val)          ;
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
        /*--------------------------------*/
        /* Cella responsabile in verde    */
        /*--------------------------------*/
//        cel_css("cel_ute", "G")           ;
    }

    /*====================================*/
    /* Test su presenza codice prodotto   */
    /*------------------------------------*/
    if (!alf_pro || alf_pro == 0)
    {
        /*--------------------------------*/
        /* Disabilitazioni                */
        /*--------------------------------*/
        $('#ubi_qta').prop("disabled",true);
        $('#acc_ub1').prop("disabled",true);
        $('#acc_ub2').prop("disabled",true);
        $('#but_cnf').prop("disabled",true);
   }
     else
    {
        /*--------------------------------*/
        /* Abilitazione ubicazioni        */
        /*--------------------------------*/
        $('#ubi_qta').prop("enabled",true);
        $('#acc_ub1').prop("enabled",true);
        $('#acc_ub2').prop("enabled",true);
        $('#but_cnf').prop("enabled",true);
    }

    /*====================================*/
    /* Test su presenza quantita'         */
    /*------------------------------------*/
    var qta = $('#ubi_qta').val()         ;
    if (qta != '' || qta != 0)
    {
        /*--------------------------------*/
        /* Focus su prima ubicazione      */
        /*--------------------------------*/
        $('#acc_ub1').focus()             ;
        $('#acc_ub1').select()            ;
    }
     else
    {
        /*--------------------------------*/
        /* Focus su quantita'             */
        /*--------------------------------*/
        $('#ubi_qta').focus()             ;
        $('#ubi_qta').select()            ;
    }

    /*====================================*/
    /* Test su presenza ubicazione 1      */
    /*------------------------------------*/
    if (acc_ub1 != '')
    {
        /*--------------------------------*/
        /* Focus su ubicazione 2          */
        /*--------------------------------*/
        $('#acc_ub2').focus()             ;
        $('#acc_ub2').select()            ;
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
    /* Highlight prodotto                 */
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
        /* Normalizzazioni                */
        /*--------------------------------*/
        $('#ubi_qta').val('')             ;
        $('#acc_ub1').val('')             ;
        $('#acc_ub2').val('')             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Highlight ubicazione 1             */
    /*------------------------------------*/
    $('#acc_ub1').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
    });

    /*====================================*/
    /* Modifica quantita' e ubicazione 1  */
    /*------------------------------------*/
    $('#acc_ub1, #ubi_qta').change(function()
    {
        /*--------------------------------*/
        /* Valori campi                   */
        /*--------------------------------*/
        var acc_ub1 = $('#acc_ub1').val().trim().toUpperCase() ;
            acc_ub1 = pul_str (acc_ub1)   ;
        $('#acc_ub1').val(acc_ub1)        ;
        var qta = $('#ubi_qta').val()     ;
        /*--------------------------------*/
        /* Se codice a spazi              */
        /*--------------------------------*/
        if (acc_ub1 == '')
        {
            $(mod_frm).submit();
            return;
        }
        else if(acc_ub1 === "LTRA")
        {
            alert('Ubicazione non selezionabile')
            $('#acc_ub1').val('')         ;
            $(mod_frm).submit()           ;
            return;
        }
        /*--------------------------------*/
        /* Controllo se ubicazione        */
        /* corrisponde a prelevabile e se */
        /* la quantita' da prelevare e'   */
        /* disponibile in ubicazione      */
        /*--------------------------------*/
        var dat_tab = let_tab('#ubi.bordotab');
        var ctr_inp = inp_ctr(dat_tab, qta, acc_ub1);
        if (!ctr_inp){return}             ;
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });          

    /*====================================*/
    /* Highlight ubicazione 2             */
    /*------------------------------------*/
    $('#acc_ub2').click(function()
    {
        /*--------------------------------*/
        /* Valore selezionato             */
        /*--------------------------------*/
        $(this).select()                  ;
    });

    /*====================================*/
    /* Modifica ubicazione 2              */
    /*------------------------------------*/
    $('#acc_ub2').change(function()
    {
        /*--------------------------------*/
        /* Valore campo input pulito      */
        /*--------------------------------*/
        var acc_ub2 = $('#acc_ub2').val().toUpperCase();
            acc_ub2 = acc_ub2.trim()      ;
            acc_ub2 = pul_str (acc_ub2)   ;
        $('#acc_ub2').val(acc_ub2)        ;
        /*--------------------------------*/
        /* Se codice a spazi              */
        /*--------------------------------*/
        if (acc_ub2 == ''){return;}
        else if(acc_ub2 === "LTRA")
            {
                alert('Ubicazione nonn selezionabile')
                $('#acc_ub2').val('')     ;
                $(mod_frm).submit()       ;
                    return;
            }
    
        /*--------------------------------*/
        /* Se codice uguale a ubicazione  */
        /* di origine                     */
        /*--------------------------------*/
        var acc_ub1 = $('#acc_ub1').val().toUpperCase();
        if (acc_ub2 == acc_ub1)
        {
            $('#acc_ub2').val('')         ;
            return                        ;
        };
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var prm = 'cod_ubi=' + acc_ub2    ;
        /*--------------------------------*/
        /* Lettura                        */
        /*--------------------------------*/
        $.ajax({ type: 'POST',
                  url: "./eleubiu0",
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
                {
                    alert ('Ubicazione di DESTINAZIONE inesistente');
                    $('#acc_ub2').val('') ;
                }
                /*------------------------*/
                /* Ricarica modulo        */
                /*------------------------*/
                $(mod_frm).submit()       ;
            } 
        });    
    });          

    /*====================================*/
    /* Controllo click su ubicazione      */
    /*------------------------------------*/
    $('#ubi td').click(function()
    {
        /*--------------------------------*/
        /* Valori da riga tabella         */
        /*--------------------------------*/
        var num_cln = $(this).parent().children().index($(this));
        /*================================*/
        /* Se colonna dell'ubicazione     */
        /*--------------------------------*/
        if (num_cln == 0) {
            /*----------------------------*/
            /* Parametri                  */
            /*----------------------------*/
            var alf_pro = $('#alf_pro').val();
            var num_pro = $('#num_pro').val();
            var ubi_prh = $('#ubi_prh').val();
            var rsp_des = $('#rsp_des').val();
            /*----------------------------*/
            /* Elemento selezionato       */
            /*----------------------------*/
            var ubi_pri = $(this).text().trim();
            /*----------------------------*/
            /* Ubicazioni di transito     */
            /*----------------------------*/
            if ([ubi_prh,
                 'LSPED',
                 'BSPED', 
                  'LRIC'].includes(ubi_pri))
            {return;}
            /*----------------------------*/
            /* Tipo operazione            */
            /*----------------------------*/
            var tip_ope = "UP"            ;
            /*----------------------------*/
            /* Conferma                   */
            /*                            */
            /* Funzione di 'eleubiJ0'     */
            /*----------------------------*/
            dlg_ubi(tip_ope,
                    ubi_pri,
                    num_pro,
                    rsp_des)              ;
        }
    });

    /*====================================*/
    /* Bottone Conferma                   */
    /*------------------------------------*/
    $('#but_cnf').click(function()
    {
        /*--------------------------------*/
        /* Valori campi                   */
        /*--------------------------------*/
        var qta = $('#ubi_qta').val()     ;
        var ub1 = $('#acc_ub1').val()     ;
        var ub2 = $('#acc_ub2').val()     ;
        /*--------------------------------*/
        /* Test se tutti presenti         */
        /*--------------------------------*/
        if (qta == '' || ub1 == '' || ub2 == '')
           {return}                       ;
        /*--------------------------------*/
        /* Messaggio conferma operazione  */
        /*--------------------------------*/
        var res_msg = 'Vuoi confermare questa registrazione?';
        /*--------------------------------*/
        /* Box di conferma operazione     */
        /*--------------------------------*/
        var riga = $(this).closest("tr").find('#pri').val();
        var my_d = $("#dialog-confirm").data('riga', riga);
        $('#dialog-confirm').html(res_msg);
        $('#dialog-confirm').dialog("option","title","ATTENZIONE !").dialog('open');
    });

    /*====================================*/
    /* Bottone Trash riga                 */
    /*------------------------------------*/
    $('.del_btn').click(function()
    {
        /*--------------------------------*/
        /* Parametri                      */
        /*--------------------------------*/
        var alf_pro = $('#alf_pro').val() ;
        var num_pro = $('#num_pro').val() ;
        var rsp_des = $('#rsp_des').val() ;
        var ubi_pri = $(this).closest("tr").find("td:first").text().trim();
        /*--------------------------------*/
        /* Se ubicazione a spazi: uscita  */
        /*--------------------------------*/
        if (!ubi_pri || ubi_pri == '')
        {return};
        /*--------------------------------*/
        /* Tipo operazione                */
        /*--------------------------------*/
        var tip_ope = "DG"                ;
        /*--------------------------------*/
        /* Conferma                       */
        /*                                */
        /* Funzione di 'eleubiJ0'         */
        /*--------------------------------*/
        dlg_ubi(tip_ope,
                ubi_pri,
                num_pro,
                rsp_des)                  ;
    });

    /*====================================*/
    /* Bottone refresh                    */
    /*------------------------------------*/
    $('#but_ref').click(function()
    {
        /*--------------------------------*/
        /* Caricamento del modulo         */
        /*--------------------------------*/
        $(mod_frm).submit()               ;
    });

    /*====================================*/
    /* Bottone stampa barcode             */
    /*------------------------------------*/
    $(".barcode").click(function () {

        /*--------------------------------*/
        /* Lettura valori da input        */
        /*--------------------------------*/
        const codiceEAN   = $("#bcd_pro").val().trim();   /* Codice EAN-13 */
        const descrizione = $("#des_pro").val().trim();   /* Descrizione prodotto */
        const prodotto    = $("#alf_pro").val().trim();   /* Codice prodotto */

        /*--------------------------------*/
        /* Se mancano dati obbligatori    */
        /*--------------------------------*/
        if (!codiceEAN || !descrizione || !prodotto) {
            alert("Dati mancanti.");
            return;
        }

        /*--------------------------------*/
        /* Contenuto dinamico del dialog  */
        /* per richiesta quantita' copie  */
        /*--------------------------------*/
        const contenutoDialog = `
            <label for="quantita_etichette">Numero etichette da stampare:</label><br>
            <div style="text-align: center; margin-top: 5px;">
            <input type="number" id="quantita_etichette" min="1" value="1" 
                    style="width: 80px; text-align: center;">
            </div>
        `;

        /*--------------------------------*/
        /* Inserimento contenuto nel box  */
        /* e titolo dinamico              */
        /*--------------------------------*/
        $("#dialog-confirm").html(contenutoDialog);
        $("#dialog-confirm").dialog("option", "title", `Stampa etichette - ${descrizione}`);

        /*--------------------------------*/
        /* Definizione bottoni dialog     */
        /*--------------------------------*/
        $("#dialog-confirm").dialog("option", "buttons", {

            /*----------------------------*/
            /* Azione di stampa           */
            /*----------------------------*/
            "Stampa": function () {
                const quantita = parseInt($("#quantita_etichette").val());

                /*------------------------*/
                /* Controllo quantita'    */
                /*------------------------*/
                if (!quantita || quantita <= 0) {
                    alert("Inserisci un numero valido.");
                    return;
                }

                /*------------------------*/
                /* Generazione stringa ZPL*/
                /*------------------------*/
                const zpl = generaZPL(codiceEAN, descrizione, prodotto, quantita);
                console.log("ZPL GENERATO:", zpl);

                /*------------------------*/
                /* Preparazione POST      */
                /*------------------------*/
                const dati = new URLSearchParams();
                dati.append("ZPL", zpl);

                /*------------------------*/
                /* Invio richiesta stampa */
                /* al backend CGI         */
                /*------------------------*/
                fetch("./elezpl01", {
                    method: "POST",
                    headers: { "Content-Type": "application/x-www-form-urlencoded" },
                    body: dati
                })
                .then(res => res.text())              /* Risposta server */
                .then(msg => {
                    alert(msg);                        /* Esito stampa */
                })
                .catch(err => {
                    console.error(err);
                    alert("Errore durante l'invio al server.");
                });

                /*------------------------*/
                /* Chiusura dialog stampa */
                /*------------------------*/
                $(this).dialog("close");
            },

            /*----------------------------*/
            /* Annulla stampa             */
            /*----------------------------*/
            "Annulla": function () {
                $(this).dialog("close");
            }
        });

        /*--------------------------------*/
        /* Apertura dialog e focus input  */
        /*--------------------------------*/
        $("#dialog-confirm").dialog("open");
        $("#quantita_etichette").focus().select();

    });
            
/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});

/*========================================*/
/* FUNZIONI                               */
/*========================================*/
/*========================================*/
/* Funzione generazione file ZPL          */
/* per stampa etichetta EAN + prodotto    */
/*----------------------------------------*/
/* Parametri:                             */
/* - ean : codice EAN-13                  */
/* - desc: descrizione prodotto           */
/* - prod: codice prodotto (alfanumerico) */
/* - copie: numero di etichette da stampare */
/*========================================*/
function generaZPL(ean, desc, prod, copie) {
    let zpl = "";                         /* Stringa finale contenente ZPL */

    for (let i = 0; i < copie; i++) {
        zpl += `
        ^XA                                     
        ^PW560                                   /* Larghezza etichetta (70mm = 560 dots) */
        ^LL288                                   /* Altezza etichetta (36mm = 288 dots) */

        ^FO130,20                                /* Posizionamento codice a barre */
        ^BY3,3,100                               /* Spessore barre, rapporto, altezza */
        ^BEN,100,Y,N                             /* Barcode EAN13, con testo visibile */
        ^FD${ean}^FS                             /* Codice EAN da stampare */

        ^FO0,180                                 /* Posizionamento codice prodotto */
        ^FB560,1,5,C,0                           /* Field block centrato, spaziatura 5 dots */
        ^A0N,40,40                               /* Font grande per codice prodotto */
        ^FD${prod}^FS                            /* Codice prodotto stampato */

        ^FO0,230                                 /* Posizionamento descrizione prodotto */
        ^FB560,1,0,C,0                           /* Field block centrato, 1 riga, no spazio righe */
        ^A0N,25,25                               /* Font medio per descrizione prodotto */
        ^FD${desc}^FS                            /* Descrizione prodotto stampata */

        ^XZ                                      /* Fine etichetta */
        `;
    }
    return zpl;                          /* Ritorno stringa completa */
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
/* Funzione di pulizia stringa            */
/*                                        */
/* N.B.: usata per togliere caratteri non */
/*       alfabetici o numerici            */
/*----------------------------------------*/
function pul_str(inp_str)
{
    return inp_str.replace(/[^a-zA-Z0-9]/g, '');
}; 

/*========================================*/
/* Funzione lettura ubicazioni e giacenza */
/* da tabella                             */
/*----------------------------------------*/
function let_tab(tab_ubi)
{
    /*------------------------------------*/
    /* Array per contenere informazioni   */
    /* tabella                            */
    /*------------------------------------*/
    var arr_dat = [];
    /*------------------------------------*/
    /* Seleziono le righe della tabella   */
    /*------------------------------------*/
    $(`${tab_ubi} tbody tr`).each(function () 
    {
        /*--------------------------------*/
        /* Estraggo i primi 2 td di ogni  */
        /* riga                           */
        /*--------------------------------*/
        var ubi = $(this).find('td').eq(0).text().trim();
        var qta = $(this).find('td').eq(1).text().trim();
        /*--------------------------------*/
        /* Aggiungi alla lista solo se i  */
        /* dati sono presenti             */
        /*--------------------------------*/
        if (ubi && qta) {
            arr_dat.push({
                ubi_tab: ubi,
            /*----------------------------*/
            /* Converto in numero la      */
            /* quantita'                  */
            /*----------------------------*/
                qta_tab: qta
            });
        }
    });
    /*------------------------------------*/
    /* Rimuovo separatori per migliaia    */
    /*------------------------------------*/
    arr_dat.forEach(item => {
        item.qta_tab = parseFloat(item.qta_tab.replace('.', ''));
    });
    return arr_dat;
};    

/*========================================*/
/* Funzione controllo ubcazione e         */
/* quantita' prelevabile                  */
/*----------------------------------------*/
function inp_ctr(dat_tab, qta, ubi_acc)
{
    /*------------------------------------*/
    /* Cerco nell'array se presente       */
    /* l'ubicazione inserita              */
    /*                                    */
    /* Il find ritorna true o false       */
    /* e i valori trovati                 */
    /*------------------------------------*/
    var ubi_pre = dat_tab.find(item => item.ubi_tab === ubi_acc.toUpperCase());
    if (ubi_pre && (qta > ubi_pre.qta_tab))
    {
        alert("Quantita superiore a quella in giacenza");
        $('#ubi_qta').select()            ;
        /*--------------------------------*/
        /* Disabilito temporaneamente     */
        /* il campo                       */
        /*--------------------------------*/
        $('#acc_ub2').prop('disabled', true);
        $('#acc_ub2').val('')             ;
        return false;
    }
    else if(!ubi_pre)
    {
        alert("Ubicazione non presente tra le disponibili");
        $('#acc_ub1').select()            ;
        /*--------------------------------*/
        /* Disabilito temporaneamente     */
        /* il campo e lo pulisco          */
        /*--------------------------------*/
        $('#acc_ub2').prop('disabled', true);
        $('#acc_ub2').val('')             ;            
        return false;
    }
    else
    {
        $('#acc_ub2').prop('disabled', false);
        return true;           
    }
}