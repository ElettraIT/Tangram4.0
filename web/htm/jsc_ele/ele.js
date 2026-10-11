/*========================================*/
/* ele - Javascript - by WIP              */
/* -------------------------------------- */
/* Aggiornamento   : 08/09/2023           */
/*========================================*/

/*========================================*/
/* Controllo dei dati                     */
/*----------------------------------------*/
$(document).ready(function()
{
    /*====================================*/
    /* Click nella tabella menu           */
    /*------------------------------------*/
    $('body').on('click','.bordotab > tbody > tr > td', function()
    {
        /*--------------------------------*/
        /* Indirizzamento                 */
        /*--------------------------------*/
        var link = $(this).parent().data('url');
        window.location.href = link       ;
    });

    /*====================================*/
    /* Test di orientamento pagina        */
    /* da perfezionare e testare          */
    /*------------------------------------*/

//    if(window.innerHeight > window.innerWidth){
//        alert("portrait");
//    };
//    if(window.innerWidth > window.innerHeight){
//        alert("landscape");
//    };

/*----------------------------------------*/
/* Fine controllo dei dati                */
/*========================================*/
});
