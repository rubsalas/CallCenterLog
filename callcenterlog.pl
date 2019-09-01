/*****************************************************************************/
/*                                                                           */
/* CE3104 - Lenguajes, Compiladores e Int�rpretes                            */
/* Grupo 02                                                                  */
/* Tarea #2 - CallCenterLog                                                  */
/* Fabi�n Crawford Barquero - 2013052995                                     */
/* Rub�n Salas Ram�rez - 2017164846                                          */
/*                                                                           */
/* Since: 28/08/19                                                           */
/* Version: 1.0                                                              */
/*                                                                           */
/*****************************************************************************/


/*****************************************************************************/
% Formato de entradas:
%
%                                         |
% -> inicio                               | -> pregunta
% -> fin                                  | -> problema
%                                         | -> referencia
% entrada([[key],[                        |
%              [frase,1],                 | entrada([[key,subkey],[
%              [frase,2],                 |              [frase,1],
%              ...                        |              [frase,2],
%              [frase,N]                  |              ...
%          ]]).                           |              [frase,N]
%                                         |          ]]).
%                                         |
%                                         |
% -------------------------------------------------------------------------
%
%
% -> causa
% -> solucion
%
% entrada([[key,subkey],[
%              [problema1/causa1,[
%                  [causa/solucion,1],
%                  [causa/solucion,2],
%                  ...
%                  [causa/solucion,N]
%               ]],
%              [problema2/causa2,[
%                  ...
%               ]],
%              ...
%              [problemaN/causaN,[
%                  ...
%               ]]
%          ]]).
%
%
% ------------------------------------------------------------------------
%
%
% -> referencia
%
%input([[key,subkey],[
%           [[problema1],[
%                [[descripcion,1],[link1]],
%                [[descripcion,2],[link2]],
%                ...
%                [[descripcion,N],[linkN]],
%            ]],
%            [[problema2],[
%                ...
%            ]],
%            ...
%            [[problemaN],[
%                ...
%            ]]
%       ]]).
%
%
% ------------------------------------------------------------------------





/*
 * Inicio
 */

%Inicio de Conversacion
input([[inicio],[
           [hola],
           [buenos,dias],
           [buenas,tardes],
           [buenas,noches],
           [buen,dia],
           [feliz,dia]
      ]]).



/*
 * Fin
 */

%Fin de Conversacion
input([[fin],[
           [gracias],
           [muchas,gracias],
           [muchisimas,gracias],
           [adios],
           [nos,vemos],
           [saludos],
           [hasta,pronto],
           [hasta,luego],
           [hasta,ma�ana],
           [hasta,la,proxima]
       ]]).



/*
 * Preguntas
 */

%Preguntas: Interrogativos
input([[pregunta,interrogativo],[
           [que],
           [cual],
           [por,que],
           [con,que],
           [para,que],
           [donde],
           [como],
           [cuanto],
           [cuanta]
        ]]).

%Preguntas: Productos
input([[pregunta,producto],[
           [macbook],
           [iphone],
           [apple,watch],
           [app,store],
           [apple,music],
           [apple,cloud]
        ]]).



/*
 * Problemas
 */

%Problemas: Macbook
input([[problema,macbook],[
           [no,se,reconocen,unidades,externas],
           [se,reinicia,por,cuenta,propia],
           [algunos,archivos,estan,desaparecidos]
       ]]).

%Problemas: iPhone
input([[problema,iphone],[
           [se,produce,una,respuesta,lenta],
           [no,carga],
           [no,responde]
       ]]).

%Problemas: Apple Watch
input([[problema,applewatch],[
           [el,bluetooth,es,inconsistente],
           [se,produce,una,respuesta,lenta],
           [la,bateria,se,descarga,rapidamente]
       ]]).

%Problemas: App Store
input([[problema,appstore],[
           [no,se,pueden,bajar,aplicaciones],
           [no,se,conecta],
           [las,aplicaciones,estan,desactualizadas]
       ]]).

%Problemas: Apple Music
input([[problema,applemusic],[
           [la,aplicacion,no,funciona],
           [no,se,reproducen,canciones],
           [no,se,guardan,canciones]
       ]]).



/*
 * Causas
 */

%Causas: Macbook
input([[causa,macbook],[
           [[no,se,reconocen,unidades,externas],[
                [se,encuentra,desconectado],
                [no,se,enciende],
                [no,se,encuentra,en,el,buscador]
            ]],
           [[se,reinicia,por,cuenta,propia],[
                [se,encuentran,corriendo,muchos,programas,a,la,vez],
                [algun,driver,",",plug-in,o,firmware,esta,desactualizado],
                [algun,dispositivo,periferico,no,es,compatible]
            ]],
           [[algunos,archivos,estan,desaparecidos],[
                [no,esta,donde,estaba,guardado],
                [el,folder,esta,vacio],
                [el,archivo,esta,corrupto]
            ]]
       ]]).

%Causas: iPhone
input([[causa,iphone],[
           [[se,produce,una,respuesta,lenta],[
                [el,almacenamiento,esta,lleno],
                [la,memoria,ram,es,insuficiente],
                [el,ios,esta,desactualizado],
                [la,bateria,esta,da�ada]
            ]],
           [[no,carga],[
                [el,cargador,esta,da�ado],
                [el,puerto,de,carga,esta,comprometido],
                [el,iOS,esta,desactualizado],
                [la,bateria,esta,da�ada]
            ]],
           [[no,responde],[
                [se,golpeo],
                [alguna,aplicacion,paro,de,funcionar],
                [se,cambio,algun,ajuste],
                [la,pantalla,esta,quebrada]
            ]]
       ]]).

%Causas: Apple Watch
input([[causa,applewatch],[
           [[el,bluetooth,es,inconsistente],[
                [se,encuentra,apagado],
                [no,se,escucha,la,musica]
            ]],
           [[se,produce,una,respuesta,lenta],[
                [hay,muchas,aplicaciones,abiertas],
                [se,usa,el,wifi,para,conectar,con,el,iPhone],
                [el,watchOS,esta,desactualizado]
            ]],
           [[la,bateria,se,descarga,rapidamente],[
                [se,reciben,muchas,notificaciones],
                [se,usa,el,wifi,para,conectar,con,el,iPhone],
                [el,monitoreo,del,ritmo,del,corazon,esta,en,uso]
            ]]
       ]]).

%Causas: App Store
input([[causa,appstore],[
           [[no,se,pueden,bajar,aplicaciones],[
                [el,almacenamiento,esta,lleno],
                [no,tiene,fondos],
                [no,tiene,apple,id],
                [se,estan,utilizando,los,datos,celulares],
                [el,sistema,operativo,esta,desactualizado]
            ]],
           [[no,se,conecta],[
                [no,hay,conexion,a,internet],
                [la,aplicacion,no,inicia],
                [el,sistema,operativo,esta,desactualizado]
            ]],
           [[las,aplicaciones,estan,desactualizadas],[
                [no,hay,conexion,a,internet],
                [el,almacenamiento,esta,lleno],
                [no,es,actualizada,automaticamente],
                [no,es,compatible,actualmente],
                [no,es,actualizada,actualmente]
            ]]
       ]]).

%Causas: Apple Music
input([[causa,applemusic],[
           [[la,aplicacion,no,funciona],[
                [no,hay,fondos,suficientes,para,pagar,la,suscripcion],
                [no,hay,conexion,a,internet],
                [el,sistema,operativo,esta,desactualizado]
            ]],
           [[no,se,reproducen,canciones],[
                [la,cancion,no,esta,disponible],
                [no,cargan,las,canciones],
                [no,suenan,las,canciones]
            ]],
           [[no,se,guardan,canciones],[
                [la,cancion,no,esta,disponible],
                [el,almacenamiento,esta,lleno],
                [no,hay,conexion,a,internet]
            ]]
       ]]).



/*
 * Soluciones
 */

%Soluciones: Macbook
input([[solucion,macbook],[
           [[se,encuentra,desconectado],[
                [conectelo,en,el,puerto,deseado]
            ]],
           [[no,se,enciende],[
                [conectelo,en,algun,otro,puerto],
                [intente,probarlo,en,otra,maquina],
                [llevelo,a,revisar,si,continua,sin,encender]
            ]],
           [[no,se,encuentra,en,el,buscador],[
                [cambie,de,puerto,el,dispositivo],
                [desconecte,el,dispositivo,y,reinicie,el,sistema],
                [instale,los,drivers,necesarios,del,dispositivo]
            ]],
           [[se,encuentran,corriendo,muchos,programas,a,la,vez],[
                [incremente,el,espacio,de,memoria],
                [inicie,la,mac,en,modo,seguro,para,correr,un,proceso,de,limpiado],
                [deshabilite,el,software,o,la,aplicacion,que,pudo,causar,el,problema]
            ]],
           [[algun,driver,",",plug-in,o,firmware,esta,desactualizado],[
                [actualice,el,driver,",",plug-in,o,firmware,que,este,desactualizado],
                [deshabilite,el,software,o,la,aplicacion,que,pudo,causar,el,problema]
            ]],
           [[algun,dispositivo,periferico,no,es,compatible],[
                [verifique,que,los,dispositivos,perifericos,puedan,utilizarse,con,su,mac],
                [cambie,el,dispositivo,periferico,que,no,sea,compatible]
            ]],
           [[no,esta,donde,estaba,guardado],[
                [busque,en,los,archivos,borrados],
                [busque,en,los,archivos,utilizados,recientemente]
            ]],
           [[el,folder,esta,vacio],[
                [busque,en,los,archivos,borrados],
                [cree,un,respaldo,de,los,archivos,que,usted,considere,necesarios]
            ]],
           [[el,archivo,esta,corrupto],[
                [cree,un,respaldo,de,los,archivos,que,usted,considere,necesarios],
                [guarde,en,un,disco,externo,los,archivos,que,usted,considere,necesarios],
                [utilice,iCloud,para,hacer,un,respaldo,o,guardar,archivos]
            ]]
       ]]).

%Soluciones: iPhone
input([[solucion,iphone],[
           [[el,almacenamiento,esta,lleno],[
                [elimine,fotos,y,videos,de,la,galeria],
                [borre,aplicaciones,que,no,utilice,frecuentemente]
            ]],
           [[la,memoria,ram,es,insuficiente],[
                [cierre,aplicaciones,que,no,necesite,en,este,momento],
                [borre,aplicaciones,que,no,utilice,frecuentemente]
            ]],
           [[el,ios,esta,desactualizado],[
                [actualice,el,ios,a,la,ultima,version,disponible]
            ]],
           [[la,bateria,esta,da�ada],[
                [lleve,a,revisar,el,dispositivo,para,verificar,el,estado,de,su,bateria],
                [cambie,la,bateria,del,dispositivo]
            ]],
           [[el,cargador,esta,da�ado],[
                [pruebe,cargar,el,dispositivo,con,un,nuevo,cargador],
                [pruebe,cargar,el,dispositivo,en,otra,fuente,de,poder]
            ]],
           [[el,puerto,de,carga,esta,comprometido],[
                [limpie,el,puerto,de,carga],
                [lleve,a,revision,el,dispositivo,para,verificar,el,estado,de,su,puerto]
            ]],
           [[se,golpeo],[
                [apague,y,reinicie,el,dispositivo],
                [lleve,a,revision,el,dispositivo,para,verificar,que,no,se,haya,da�ado,algun,componente]
            ]],
           [[alguna,aplicacion,paro,de,funcionar],[
                [apague,y,reinicie,el,dispositivo],
                [borre,la,aplicacion,que,pudo,causar,el,problema]
            ]],
           [[se,cambio,algun,ajuste],[
                [busque,el,ajuste,modificado,y,retornelo,a,como,se,encontraba],
                [reinicie,el,dispositivo,de,fabrica]
            ]],
           [[la,pantalla,esta,quebrada],[
                [lleve,a,revision,el,dispositivo,para,verificar,que,la,pantalla,no,comprometa,su,uso],
                [cambie,la,pantalla,quebrada]
            ]]
       ]]).

%Soluciones: Apple Watch
input([[solucion,applewatch],[
           [[se,encuentra,apagado],[
                [prenda,y,apague,el,bluetooth,del,celular],
                [reconecte,el,dispositivo,con,el,iPhone,y,los,audifonos,si,es,el,caso]
            ]],
           [[no,se,escucha,la,musica],[
                [descargue,la,musica,en,el,dispositivo,para,que,esta,no,venga,desde,el,iPhone],
                [desconecte,y,reconecte,los,audifonos,con,el,dispositivo],
                [reinicie,el,dispositivo]
            ]],
           [[hay,muchas,aplicaciones,abiertas],[
                [limite,la,cantidad,de,aplicaciones,que,pueden,correr,al,mismo,tiempo],
                [limite,la,cantidad,de,notificaciones,que,puede,recibir]
            ]],
           [[se,usa,el,wifi,para,conectar,con,el,iPhone],[
                [utilice,el,bluetooth,para,conectarse,con,el,iPhone],
                [reinicie,el,dispositivo]
            ]],
           [[el,watchOS,esta,desactualizado],[
                [actualice,el,watchOS,del,dispositivo,desde,el,iPhone]
            ]],
           [[se,reciben,muchas,notificaciones],[
                [limite,la,cantidad,de,notificaciones,que,puede,recibir],
                [limite,la,cantidad,de,aplicaciones,que,pueden,correr,al,mismo,tiempo]
            ]],
           [[el,monitoreo,del,ritmo,del,corazon,esta,en,uso],[
                [active,el,modo,de,ahorro,de,energia]
            ]]
       ]]).

%Soluciones: App Store
input([[solucion,appstore],[
           [[el,almacenamiento,esta,lleno],[
                [elimine,fotos,y,videos,de,la,galeria],
                [borre,aplicaciones,que,no,utilice,frecuentemente]
            ]],
           [[no,tiene,fondos],[
                [agregue,una,tarjeta,credito,o,debito],
                [agregue,una,cuenta,de,paypal],
                [redima,una,tarjeta,de,regalo,o,codigo]
            ]],
           [[no,tiene,apple,id],[
                [creese,un,apple,id]
            ]],
           [[se,estan,utilizando,los,datos,celulares],[
                [conectese,al,wifi],
                [active,la,opcion,de,descargar,aplicaciones,con,los,datos,mobiles]
            ]],
           [[el,sistema,operativo,esta,desactualizado],[
                [actualice,el,sistema,operativo,a,la,ultima,version,disponible]
            ]],
           [[no,hay,conexion,a,internet],[
                [conectese,a,una,red,wifi,o,utilice,datos,mobiles],
                [active,el,modo,de,avion,y,luego,espere,",",para,salir,de,este,",",uno,a,dos,minutos,despues]
            ]],
           [[la,aplicacion,no,inicia],[
                [cierre,su,sesion,y,vuelva,a,iniciar,sesion],
                [cierre,y,vuelva,a,abrir,la,aplicacion],
                [verifique,si,apple,tiene,problemas,con,el,servidor,del,app,store]
            ]],
           [[no,es,actualizada,automaticamente],[
                [active,la,opcion,para,actualizar,las,aplicaciones,automaticamente,en,ajustes]
            ]],
           [[no,es,compatible,actualmente],[
                [actualice,el,sistema,operativo,a,la,ultima,version,disponible],
                [el,dispositivo,puede,no,soportar,las,nuevas,versiones,de,la,aplicacion]
            ]],
           [[no,es,actualizada,actualmente],[
                [contacte,al,desarrollador,de,la,aplicacion],
                [busque,una,aplicacion,parecida,que,cumpla,sus,necesidades]
            ]]
       ]]).

%Soluciones: Apple Music
input([[solucion,applemusic],[
           [[no,hay,fondos,suficientes,para,pagar,la,suscripcion],[
                [cambie,de,plan,mensual,a,uno,que,cumpla,con,su,presupuesto],
                [agregue,una,tarjeta,credito,o,debito],
                [agregue,una,cuenta,de,paypal],
                [redima,una,tarjeta,de,regalo,o,codigo]
            ]],
           [[no,hay,conexion,a,internet],[
                [conectese,a,una,red,wifi,o,utilice,datos,mobiles],
                [active,el,modo,de,avion,y,luego,espere,",",para,salir,de,este,",",uno,a,dos,minutos,despues]
            ]],
           [[el,sistema,operativo,esta,desactualizado],[
                [actualice,el,sistema,operativo,a,la,ultima,version,disponible]
            ]],
           [[la,cancion,no,esta,disponible],[
                [acepte,el,uso,de,iCloud,music,library,en,ajustes],
                [la,cancion,puede,que,no,se,encuentre,disponible,en,su,pais,o,region]
            ]],
           [[no,cargan,las,canciones],[
                [desconectese,de,la,red,y,vuelvase,a,conectar,al,wifi,o,a,los,datos,moviles],
                [apague,y,reinicie,el,dispositivo]
            ]],
           [[no,suenan,las,canciones],[
                [revise,que,el,dispositivo,tenga,volumen],
                [reinicie,la,aplicacion],
                [borre,y,reinstale,la,aplicacion]
            ]],
           [[el,almacenamiento,esta,lleno],[
                [elimine,canciones,que,no,seran,reproducidas,recientemente],
                [elimine,fotos,y,videos,de,la,galeria],
                [borre,aplicaciones,que,no,utilice,frecuentemente]
            ]]
       ]]).



/*
 * Referencias
 */

%Referencias: Macbook
input([[referencia,macbook],[
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
            [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]]
       ]]).

%Referencias: iPhone
input([[referencia,iphone],[
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
            [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]]
       ]]).

%Referencias: Apple Watch
input([[referencia,applewatch],[
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
            [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]]
       ]]).

%Referencias: App Store
input([[referencia,appstore],[
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
            [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]]
       ]]).

%Referencias: Apple Music
input([[referencia,applemusic],[
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
            [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]],
           [[problema],[
                [[descripcion],[link]],
                [[descripcion],[link]]
            ]]
       ]]).



/*****************************************************************************/
%
% Main de CallCenterLog
%
%


callCenterLog :- write("Usuario:       "),
                 read(X),
                 %repeat,
                 %write("CallCenterLog: "),
                 write(X).


:- callCenterLog.
