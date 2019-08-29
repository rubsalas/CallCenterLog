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
% ------------------------------------------------------------------------
%



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
           [hasta,mañana],
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
           [],
           [], %FALTA
           []
       ]]).

%Problemas: iPhone
input([[problema,iphone],[
           [respuesta,lenta],
           [no,carga],
           [no,responde]
       ]]).

%Problemas: Apple Watch
input([[problema,applewatch],[
           [bluetooth,inconsistente],
           [respuesta,lenta],
           [bateria,descargada,rapidamente]
       ]]).

%Problemas: App Store
input([[problema,appstore],[
           [],
           [], %FALTA
           []
       ]]).

%Problemas: Apple Music
input([[problema,applemusic],[
           [aplicacion,no,funciona],
           [no,se,reproducen,canciones],
           [no,se,guardan,canciones]
       ]]).

%Problemas: Apple Cloud
input([[problema,applecloud],[
           [],
           [], %FALTA
           []
       ]]).



/*
 * Causas
 */

%Causas: Macbook
input([[causa,macbook],[
           %FALTA
           [[problema],[
                [],
                [],
                [],
                []
            ]],
           [[problema],[
                [],
                [],
                [],
                []
            ]],
           [[problema],[
                [],
                [],
                [],
                []
            ]]
       ]]).

%Causas: iPhone
input([[causa,iphone],[
           [respuestalenta,[
                [almacenamiento,lleno],
                [ram,insuficiente],
                [ios,desactualizado],
                [bateria,dañada]
            ]],
           [[nocarga],[
                [],
                [],
                [],
                []
            ]],
           [[noresponde],[
                [],
                [],
                [],
                []
            ]]

       ]]).

%Causas: Apple Watch
input([[causa,applewatch],[
           [[bluetoothinconsistente],[
                [],
                [],
                [],
                []
            ]],
           [[respuestalenta],[
                [],
                [],
                [],
                []
            ]],
           [[bateriadescargadarapidamente],[
                [],
                [],
                [],
                []
            ]]
       ]]).

%Causas: App Store
input([[causa,appstore],[
           %FALTA
           [[problema],[
                [],
                [],
                [],
                []
            ]],
           [[problema],[
                [],
                [],
                [],
                []
            ]],
           [[problema],[
                [],
                [],
                [],
                []
            ]]
       ]]).

%Causas: Apple Music
input([[causa,applemusic],[
           [[aplicacionnofunciona],[
                [],
                [],
                [],
                []
            ]],
           [[nosereproducencanciones],[
                [],
                [],
                [],
                []
            ]],
           [[noseguardancanciones],[
                [],
                [],
                [],
                []
            ]]
       ]]).

%Causas: Apple Cloud
input([[causa,applecloud],[
           %FALTA
           [[problema],[
                [],
                [],
                [],
                []
            ]],
           [[problema],[
                [],
                [],
                [],
                []
            ]],
           [[problema],[
                [],
                [],
                [],
                []
            ]]
       ]]).



/*
 * Soluciones
 */

%Soluciones: Macbook
input([[solucion,macbook],[
           %FALTA
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]]
       ]]).

%Soluciones: iPhone
input([[solucion,iphone],[
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]]
       ]]).

%Soluciones: Apple Watch
input([[solucion,applewatch],[
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]]
       ]]).

%Soluciones: App Store
input([[solucion,appstore],[
           %FALTA
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]]
       ]]).

%Soluciones: Apple Music
input([[solucion,applemusic],[
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]]
       ]]).

%Soluciones: Apple Cloud
input([[solucion,applecloud],[
           %FALTA
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]],
           [[causa],[
                [],
                [],
                [],
                []
            ]]
       ]]).



/*
 * Referencias
 */

%Referencias: Macbook
input([[referencia,macbook],[
           [] %FALTA
       ]]).

%Referencias: iPhone
input([[referencia,iphone],[
           [] %FALTA
       ]]).

%Referencias: Apple Watch
input([[referencia,applewatch],[
           [] %FALTA
       ]]).

%Referencias: App Store
input([[referencia,appstore],[
           [] %FALTA
       ]]).

%Referencias: Apple Music
input([[referencia,applemusic],[
           [] %FALTA
       ]]).

%Referencias: Apple Cloud
input([[referencia,applecloud],[
           [] %FALTA
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
