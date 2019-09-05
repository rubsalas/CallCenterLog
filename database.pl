/*****************************************************************************/
/*                                                                           */
/* CE3104 - Lenguajes, Compiladores e Intérpretes                            */
/* Grupo 02                                                                  */
/* Tarea #2 - CallCenterLog                                                  */
/* Fabián Crawford Barquero - 2013052995                                     */
/* Rubén Salas Ramírez - 2017164846                                          */
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
%              [[problema1/causa1],[
%                  [causa/solucion,1],
%                  [causa/solucion,2],
%                  ...
%                  [causa/solucion,N]
%               ]],
%              [[problema2/causa2],[
%                  ...
%               ]],
%              ...
%              [[problemaN/causaN],[
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
%           [[problema,1],[
%                [[descripcion,1],[link1]],
%                [[descripcion,2],[link2]],
%                ...
%                [[descripcion,N],[linkN]],
%            ]],
%            [[problema,2],[
%                ...
%            ]],
%            ...
%            [[problema,N],[
%                ...
%            ]]
%       ]]).
%
%
% ------------------------------------------------------------------------


/*
 * Base de Datos para BNFs
 */


%Determinantes
%Todos los posibles
determinantes([
               %Personales
               yo,me,mi,
               tu,vos,te,ti,
               usted,lo,la,
               el,ella,le,se,
               nosotros,nosotras,nos,
               vosotros,vosotras,os,
               ustedes,los,las,
               ellos,ellas,les,
               %Posesivos
               mis,tus,sus,su,
               nuestro,nuestra,
               nuestros,nuestras,
               vuestro,vuestra,
               vuestros,vuestras,
               mio,mia,mios,mias,
               tuyo,tuya,tuyos,tuyas,
               suyo,suya,suyos,suyas,
               %Reflexivos
               se,
               %Relativos/Interrogativos
               que,cual,cuales,
               quien,quienes,cuyo,cuya,
               cuyos,cuyas,cuanto,cuanta,
               cuantos,cuantas,
               %Demostrativos
               este,ese,aquel,
               estos,esos,aquellos,
               esta,esa,aquella,
               estas,esas,aquellas,
               esto,eso,aquello,
               %Indefinidos
               algun,alguno,alguna,algunos,algunas,
               otro,otra,otros,otras,
               mucho,mucha,muchos,muchas,
               poco,poca,pocos,pocas,
               demasiado,demasiadas,demasiados,demasiadas,
               todo,toda,todos,todas,
               cierto,cierta,ciertos,ciertas,
               tanto,tanta,tantos,tantas,
               bastante,bastantes,cualquier,cualesquiera,
               quienquiera,quienesquiera,
               varios,varias,ningun,ninguna,
               alguien,algo,mas,menos,nada,nadie,
               cualquiera,cada
              ]).



%Sujetos Unicos
%Solo estan una vez, se encuentran todos los de los dispositivos
sujetos([
    actualizaciones,ahorro,almacenamiento,ajuste,ajustes,aplicacion,aplicaciones,apple,appleID,archivo,archivos,audifonos,avion,
    bateria,bluetooth,buscador,
    cancion,canciones,cargador,codigo,componente,compras,comunidad,conexion,conexiones,copias,corazon,credito,cuenta,
    datos,debito,desarrollador,disco,dispositivo,driver,drivers,duracion,
    energia,espacio,
    firmware,folder,fondos,fotos,fuente,
    galeria,
    iCloud,internet,iOS,iPhone,
    juegos,
    library,
    maquina,memoria,minutos,modo,monitoreo,moviles,music,musica,
    necesidades,notificaciones,
    opcion,
    pais,pantalla,pasos,paypal,plan,plug-in,presupuesto,problema,programas,proceso,puerto,puertos,
    red,regalo,region,rendimiento,respaldo,respuesta,ritmo,
    servicio,servidor,sesion,sistema,software,soporte,suscripcion,suscripciones,
    tarjeta,tiempo,
    unidades,usb,
    version,versiones,vida,videos,volumen,
    watchOS,wifi
]).



%Verbos
verbos([
    abrir,acceder,acepte,active,actualice,actualizada,actualizan,actualizar,agregar,agregue,apague,aparece,aparecen,aumentar,
    bajar,borre,busque,
    cambie,cambio,carga,cargan,cargando,causar,cierre,comprometa,conecta,conectar,conectarse,conectelo,conectese,
    configurar,considere,contacte,continua,correr,corriendo,cree,creese,cumpla,
    debido,desaparecieron,descarga,descargar,descargue,desconecte,desconectese,deshabilite,
    elimine,encender,enciende,encontraba,encuentra,encuentran,encuentre,encuentro,es,
    escucha,escuchar,espere,esta,estaba,estan,este,extraviaron,
    facilitar,funciona,funcionando,funcionar,
    golpeo,guardado,guardan,guardar,guarde,
    hacer,hay,
    incremente,indicarme,indiqueme,ingresar,inicia,iniciar,inicie,instale,intente,investigar,
    limite,limpie,lleve,llevelo,logro,
    maximizar,monitoreo,
    necesite,
    pagar,parece,paro,pasar,pasarme,podria,ponerse,prenda,preparacion,probarlo,produce,pruebe,pudo,puedan,puede,pueden,puedo,
    quiere,
    reciben,recibir,reconecte,reconocen,redima,reinicia,reiniciado,reinicia,reinicie,reinicio,
    reinstale,rendir,reparacion,reproducen,reproducidas,reproducir,responde,responder,respondiendo,retornelo,revisar,revise,rinde,
    salir,se,sea,seran,son,soportar,suenan,
    tendra,tenga,tengo,teniendo,tiene,
    usa,uso,utilice,utilizando,utilizarse,
    va,vemos,venga,verificar,verifique,vuelva,vuelvase
]).



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
           [hasta,manana],
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
output([[causa,macbook],[
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
output([[causa,iphone],[
           [[se,produce,una,respuesta,lenta],[
                [el,almacenamiento,esta,lleno],
                [la,memoria,ram,es,insuficiente],
                [el,iOS,esta,desactualizado],
                [la,bateria,esta,danada]
            ]],
           [[no,carga],[
                [el,cargador,esta,danado],
                [el,puerto,de,carga,esta,comprometido],
                [el,iOS,esta,desactualizado],
                [la,bateria,esta,danada]
            ]],
           [[no,responde],[
                [se,golpeo],
                [alguna,aplicacion,paro,de,funcionar],
                [se,cambio,algun,ajuste],
                [la,pantalla,esta,quebrada]
            ]]
       ]]).

%Causas: Apple Watch
output([[causa,applewatch],[
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
output([[causa,appstore],[
           [[no,se,pueden,bajar,aplicaciones],[
                [el,almacenamiento,esta,lleno],
                [no,tiene,fondos],
                [no,tiene,appleID],
                [se,estan,utilizando,los,datos,moviles],
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
output([[causa,applemusic],[
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
output([[solucion,macbook],[
           [[se,encuentra,desconectado],[
                [conectelo,en,el,puerto,deseado]
            ]],
           [[no,se,enciende],[
                [conectelo,en,otro,puerto],
                [intente,probarlo,en,otra,maquina],
                [llevelo,a,revisar,si,continua,sin,encender]
            ]],
           [[no,se,encuentra,en,el,buscador],[
                [cambie,de,puerto,el,dispositivo],
                [desconecte,el,dispositivo,y,reinicie,el,sistema],
                [instale,los,drivers,necesarios,del,dispositivo]
            ]],
           [[se,encuentran,corriendo,muchos,programas,a,la,vez],[
                [incremente,la,memoria,ram],
                [incremente,el,espacio,de,memoria],
                [inicie,el,dispositivo,en,modo,seguro,para,correr,un,proceso,de,limpiado],
                [deshabilite,el,software,o,la,aplicacion,que,pudo,causar,el,problema]
            ]],
           [[algun,driver,",",plug-in,o,firmware,esta,desactualizado],[
                [actualice,el,driver,",",plug-in,o,firmware,que,este,desactualizado],
                [deshabilite,el,software,o,la,aplicacion,que,pudo,causar,el,problema]
            ]],
           [[algun,dispositivo,periferico,no,es,compatible],[
                [verifique,que,los,dispositivos,perifericos,puedan,utilizarse,con,el,dispositivo],
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
                [utilice,iCloud,para,hacer,un,respaldo,o,para,guardar,archivos]
            ]]
       ]]).

%Soluciones: iPhone
output([[solucion,iphone],[
           [[el,almacenamiento,esta,lleno],[
                [elimine,fotos,y,videos,de,la,galeria],
                [borre,aplicaciones,que,no,utilice,frecuentemente]
            ]],
           [[la,memoria,ram,es,insuficiente],[
                [cierre,aplicaciones,que,no,necesite,en,el,momento],
                [borre,aplicaciones,que,no,utilice,frecuentemente]
            ]],
           [[el,iOS,esta,desactualizado],[
                [actualice,el,iOS,a,la,ultima,version,disponible]
            ]],
           [[la,bateria,esta,danada],[
                [lleve,a,revisar,el,dispositivo,para,verificar,el,estado,de,su,bateria],
                [cambie,la,bateria,del,dispositivo]
            ]],
           [[el,cargador,esta,danado],[
                [pruebe,cargar,el,dispositivo,con,un,nuevo,cargador],
                [pruebe,cargar,el,dispositivo,en,otra,fuente,de,poder]
            ]],
           [[el,puerto,de,carga,esta,comprometido],[
                [limpie,el,puerto,de,carga],
                [lleve,a,revision,el,dispositivo,para,verificar,el,estado,de,su,puerto]
            ]],
           [[se,golpeo],[
                [apague,y,reinicie,el,dispositivo],
                [lleve,a,revision,el,dispositivo,para,verificar,que,no,se,haya,danado,algun,componente]
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
output([[solucion,applewatch],[
           [[se,encuentra,apagado],[
                [prenda,y,apague,el,bluetooth,del,iPhone],
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
output([[solucion,appstore],[
           [[el,almacenamiento,esta,lleno],[
                [elimine,fotos,y,videos,de,la,galeria],
                [borre,aplicaciones,que,no,utilice,frecuentemente]
            ]],
           [[no,tiene,fondos],[
                [agregue,una,tarjeta,de,credito,o,debito],
                [agregue,una,cuenta,de,paypal],
                [redima,una,tarjeta,de,regalo,o,codigo]
            ]],
           [[no,tiene,appleID],[
                [creese,un,appleID]
            ]],
           [[se,estan,utilizando,los,datos,moviles],[
                [conectese,al,wifi],
                [active,la,opcion,de,descargar,aplicaciones,con,los,datos,moviles]
            ]],
           [[el,sistema,operativo,esta,desactualizado],[
                [actualice,el,sistema,operativo,a,la,ultima,version,disponible]
            ]],
           [[no,hay,conexion,a,internet],[
                [conectese,a,una,red,wifi,o,utilice,datos,moviles],
                [active,el,modo,de,avion,y,luego,espere,",",para,salir,de,este,",",uno,a,dos,minutos,despues]
            ]],
           [[la,aplicacion,no,inicia],[
                [cierre,su,sesion,y,vuelva,a,iniciar,sesion],
                [cierre,y,vuelva,a,abrir,la,aplicacion],
                [verifique,si,apple,tiene,problemas,con,el,servidor,del,servicio]
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
output([[solucion,applemusic],[
           [[no,hay,fondos,suficientes,para,pagar,la,suscripcion],[
                [cambie,de,plan,mensual,a,uno,que,cumpla,con,su,presupuesto],
                [agregue,una,tarjeta,de,credito,o,debito],
                [agregue,una,cuenta,de,paypal],
                [redima,una,tarjeta,de,regalo,o,codigo]
            ]],
           [[no,hay,conexion,a,internet],[
                [conectese,a,una,red,wifi,o,utilice,datos,moviles],
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
output([[referencia,macbook],[
           [[no,se,reconocen,unidades,externas],[
                [[puertos],["https://support.apple.com/es-es/HT207443"]],
                [[acceder,a,un,usb,que,no,aparece],["https://www.macworld.es/tutoriales/mac/acceder-disco-externo-no-aparece-mac-3685554/"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/mac"]]
            ]],
            [[se,reinicia,por,cuenta,propia],[
                [[preparacion,de,mac,para,reparacion],["https://support.apple.com/es-es/HT209095"]],
                [[reiniciado,debido,a,un,problema],["https://support.apple.com/es-es/HT200553"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/mac"]]
            ]],
           [[algunos,archivos,estan,desaparecidos],[
                [[copias,de,seguridad],["https://support.apple.com/es-es/mac-backup"]],
                [[configurar,iCloud,drive],["https://support.apple.com/es-es/HT204025"]],
                [[comunidad],["https://communities.apple.com/es/thread/160029078"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/mac"]]
            ]]
       ]]).

%Referencias: iPhone
output([[referencia,iphone],[
           [[se,produce,una,respuesta,lenta],[
                [[dispositivo,va,lento],["https://www.ipadizate.es/2018/07/06/como-acelerar-iphone-lento/"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/iphone"]]
            ]],
            [[no,carga],[
                [[bateria,y,rendimiento],["https://support.apple.com/es-es/HT208387"]],
                [[bateria,no,carga],["https://www.macworld.es/tutoriales/iphone/iphone-no-carga-bateria-3675545/"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/iphone"]]
            ]],
           [[no,responde],[
                [[problemas,de,pantalla],["https://support.apple.com/es-es/HT201406"]],
                [[pantalla,bloqueada],["https://support.apple.com/es-es/HT201412"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/iphone"]]
            ]]
       ]]).

%Referencias: Apple Watch
output([[referencia,applewatch],[
           [[el,bluetooth,es,inconsistente],[
                [[si,dispositivo,no,esta,conectado],["https://support.apple.com/es-es/HT205025"]],
                [[conexiones,bluetooth],["https://support.apple.com/es-es/HT204562"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/watch"]]
            ]],
            [[se,produce,una,respuesta,lenta],[
                [[aumentar,el,rendimiento],["https://www.smartwatchzone.net/acelerar-apple-watch/"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/watch"]]
            ]],
           [[la,bateria,se,descarga,rapidamente],[
                [[no,carga,o,enciende],["https://support.apple.com/es-es/HT204640"]],
                [[maximizar,duracion,y,vida,util],["https://www.apple.com/la/batteries/maximizing-performance/"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/watch"]]
            ]]
       ]]).

%Referencias: App Store
output([[referencia,appstore],[
           [[la,aplicacion,no,funciona],[
                [[suscripciones,y,compras],["https://support.apple.com/es-es/billing"]],
                [[descargar,aplicaciones,y,juegos],["https://support.apple.com/es-es/HT204266"]],
                [[aplicacion,no,se,descaga,correctamente],["https://www.applesfera.com/tutoriales/9-cosas-que-puedes-probar-si-una-app-no-se-te-actualiza-o-descarga-correctamente-en-ios"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/apps"]]
            ]],
           [[no,se,conecta],[
                [[no,se,puede,conectar,al,servicio],["https://support.apple.com/es-es/HT201400"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/apps"]]
            ]],
           [[las,aplicaciones,estan,desactualizadas],[
                [[ponerse,en,contacto,con,el,desarrollador],["https://support.apple.com/es-es/HT207959"]],
                [[actualizaciones,automaticas],["https://support.apple.com/es-es/HT202180"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/apps"]]
            ]]
       ]]).

%Referencias: Apple Music
output([[referencia,applemusic],[
           [[no,se,pueden,bajar,aplicaciones],[
                [[primeros,pasos],["https://support.apple.com/es-es/music/using-apple-music"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/music/using-apple-music"]]
            ]],
           [[no,se,reproducen,canciones],[
                [[comunidad],["https://communities.apple.com/es/thread/250086640"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/music/using-apple-music"]]
            ]],
           [[no,se,guardan,canciones],[
                [[anadir,y,descargar,musica],["https://support.apple.com/es-es/HT204839"]],
                [[escuchar,offline],["https://www.actualidadiphone.com/como-guardar-canciones-de-apple-music-para-escuchar-offline/"]],
                [[soporte,tecnico],["https://support.apple.com/es-es/music/using-apple-music"]]
            ]]
       ]]).
