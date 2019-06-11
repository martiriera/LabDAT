# PRÀCTICA 1 - Introducció
_Sergio Giménez i Martí Riera_

## Índex

* [<span>PRÀCTICA 1 - Introducció</span>](#pràctica-1---introducció)
* [Primera part](#primera-part)
* [Segona part](#segona-part)
   * [Pregunta 1](#pregunta-1)
   * [Pregunta 2](#pregunta-2)
* [Tercera part](#tercera-part)
   * [Pregunta 1](#pregunta-1-1)
   * [Pregunta 2](#pregunta-2-1)

## Primera part

1. **Feu un pàgina simple amb URL _http://soft0.upc.edu/~USER/_   (on _USER_ és el vostre nom d'usuari). Podeu copiar la[ plantilla d'exemple](http://soft0.upc.edu/web/cgi/totext.cgi/dat/practica1/plantilla.html?UTF-8) i modificar-la convenientment.**
2. **Afegiu un enllac a una nova pàgina amb URL _http://soft0.upc.edu/~USER/practica1/_. La pàgina anterior servirà d'índex per a les diferents pràctiques. Per ara, considereu només text en la nova pàgina.**

    ```html
    <P>Enllaç (<em>hyperlink</em>) a la practica 1:<A
    HREF="http://soft0.upc.edu/~ldatusr14/practica1/">pr&agrave;ctica 1</A>.</P>
    ```


3. **Afegiu un enllac des de la pàgina _http://soft0.upc.edu/~USER/practica1/_ a una nova pàgina _http://soft0.upc.edu/~USER/practica1/exemple.html_. \
 Canvieu temporalment el sufix _.html_ per _.txt_ i visiteu amb el navegador la pàgina amb el nou nom. Raoneu els efectes produits pel canvi en la presentació del contingut.**

    El contingut HTML no s’interpreta correctament per part del navegador. La presentació del contingut és errònia. Es mostren els <elements> propis del llenguatge.

4. **Canvieu temporalment el nom del directori _practica1_ per un altre. Modifiqueu adequadament les pàgines per a que quedin correctament enllaçades (quines ?). Com han de ser les URLs dels enllaços en el HTML per permetre moure directoris còmodament ?**

    Tot i canviar el nom del directori, si les rutes a les pàgines són relatives, aquestes continuen correctament enllaçades

5. **Incorporeu un enllaç a una pàgina creada dinàmicament per un CGI que presenta[ el dia i la hora](http://soft0.upc.edu/web/cgi/exemples/dia-hora.cgi) actuals del servidor. Caldrà:**
    1. **copiar el[ codi font del CGI](http://soft0.upc.edu/web/cgi/totext.cgi/web/cgi/exemples/dia-hora.cgi?UTF-8) i canviar-li el nom per tal que tingui el sufix _.cgi_.**

        Anomenem l’arxiu dia-hora.cgi

    2. **donar-li permisos d'execució amb el programa chmod.**

        ```bash
        chmod x+u
        ```
    3. **modificar la pàgina _http://soft0.upc.edu/~USER/practica1/_ incorporant la referència al CGI.**

        ```html
        <P>Enllaç a un CGI que printa data i hora <A
        HREF="dia-hora.cgi">DATA I HORA</A>.</P>
        ```


6. **Modifiqueu el codi del CGI per tal de que indiqui el tipus MIME _text/plain_. Raoneu els efectes produits.**

    Hem de canviar la línia on s'especifica el tipus de contingut

    ```html
    echo "Content-Type: text/plain"
    ```
  Obtenim el fitxer com a sequencia de caràcters (tal com al punt 3). El servidor utilitza el tipus MIME per tal de que el navegador sàpiga com llegir l’arxiu.

## Segona part

### Pregunta 1

**Usant el programa telnet, obteniu un document qualsevol amb el mètode GET de HTTP. **

```bash
$ telnet soft0.upc.edu 80
$ GET /~ldatusr14/practica1/dia-hora.cgi HTTP/1.0
$ (blank line)
```

**Indiqueu i comenteu de forma detallada cadascuna de les capçaleres, tant de la petició com de la resposta. **

La petició sols està formada per el GET on especifiquem l’ubicació del recurs a obtenir i la versió del protocol HTTP a utilitzar, en el nostre cas la 1.0. A continuació hem de deixar una línia en blanc.

Obtenim una resposta on es mostren diferents headers informatius de la petició:

`Date, Server, Last-Modified, Accept-Ranges, Content-Length, Vary, Connection, Content-Type`


### Pregunta 2

**Feu una petició amb el mètode POST al CGI “test-http”. Aquest CGI respon amb una pàgina que conté informació sobre la petició realitzada (veieu el final de la sortida). **


    $ telnet soft0.upc.edu 80
    $ POST [http://soft0.upc.edu/web/cgi/exemples/test-http.cgi](http://soft0.upc.edu/web/cgi/exemples/test-http.cgi) HTTP/1.0
    $ Content-length: 10 (Mida del text d’entrada)
    $ Content-type: text/plain
    $ (blank line)
    $ 012345678910(ENTRADA)

    HTTP/1.1 200 OK
    Date: Thu, 07 Mar 2019 18:24:40 GMT
    Server: Apache
    Vary: Accept-Encoding
    Connection: close
    Content-Type: text/html
    <HTML>
    <HEAD><TITLE>Test CGI/1.0</TITLE></HEAD>
     <BODY BGCOLOR="#FFFFFF" TEXT="#000000">
    <CENTER><H1>Resultat del test de CGI/1.0</H1></CENTER>
    <p>argc &eacute;s 1.<br>
    argv[0] &eacute;s <code>"/var/www/web/cgi/exemples/test-http.cgi"</code>.<br>
    </p>
    <p>Directori de treball: <code>"/home/pract/LabWEB/WEBprofe/www/web/cgi/exemples"</code>.<br></p>
    <p>Variables d'entorn:
    <ul>
    <li> <em>SERVER_SOFTWARE</em> = <code>Apache</code></li>
    <li> <em>SERVER_NAME</em> = <code>soft0.upc.edu</code></li>
    <li> <em>GATEWAY_INTERFACE</em> = <code>CGI/1.1</code></li>
    <li> <em>SERVER_PROTOCOL</em> = <code>HTTP/1.0</code></li>
    <li> <em>SERVER_PORT</em> = <code>80</code></li>
    <li> <em>REQUEST_METHOD</em> = <code>POST</code></li>
    <li> <em>PATH_INFO</em> = <code>(null)</code></li>
    <li> <em>PATH_TRANSLATED</em> = <code>(null)</code></li>
    <li> <em>SCRIPT_NAME</em> = <code>/web/cgi/exemples/test-http.cgi</code></li>
    <li> <em>QUERY_STRING</em> = <code></code></li>
    <li> <em>REMOTE_HOST</em> = <code>(null)</code></li>
    <li> <em>REMOTE_ADDR</em> = <code>10.0.40.103</code></li>
    <li> <em>REMOTE_USER</em> = <code>(null)</code></li>
    <li> <em>AUTH_TYPE</em> = <code>(null)</code></li>
    <li> <em>CONTENT_TYPE</em> = <code>text/plain</code></li>
    <li> <em>CONTENT_LENGTH</em> = <code>10</code></li>
    <li> <em>HTTP_ACCEPT</em> = <code>(null)</code></li>
    <li> <em>HTTP_ATTR1</em> = <code>(null)</code></li>
    </ul>
    </p>
    <p>Entrada:</p>
    <pre><em>(inici d'entrada)</em>0123456789<em>(fi d'entrada)</em></pre>

    </BODY>

    </HTML>


    Connection closed by foreign host.

Tot i haver entrat fins al 10, aquests dos últims caràcters no són a la sortida ja que hem especificat la mida del contingut a 10.

**Quines diferències hi ha respecte al mètode GET ? **

En aquest cas cal especificar el content-type i length així com l’entrada després de l’espai en blanc.


## Tercera part


### Pregunta 1

**Farem un CGI que genera dinàmicament el document de resposta però sense cap tractament de l'entrada. Un bon exemple pot ser el d'un comptador de visites.**

**Cada cop que s'accedeix el CGI comptador, incrementa el valor del comptador i genera un document (una pàgina HTML per exemple) amb el nou valor. Caldrà mantenir el valor del comptador de forma persistent (en algun fitxer).**

**Realitzeu aquest CGI comptador. Podeu programar-lo en C, en _script_ o qualsevol llenguatge de programació que conegueu (per aquest cas simple, la programació en _script_ de la _shell_ sembla la més adequada).**

**Autodocumenteu el programa el millor que pogueu (introdüint els comentaris adeqüats).**


```bash
#!/bin/sh

v=$(cat visites.txt)
#Obrim el fitxer on es guarden les visites i assignem aquestes a "v"
v=$(expr $v + 1)
#Evaluem "v" i incrementem una vista
echo $v > visites.txt
#Actualitzem el fitxer de visites

#HTML necessari per tal de que el navegador pugui llegir el cgi
echo "Content-Type: text/html"
echo
echo "<HTML><HEAD>"
echo "<TITLE>CGI que dona el numero de visites...</TITLE>"
echo "</HEAD>"
echo "<BODY>"
echo "<H1>Numero de visites: "
echo "</H1>"
echo "$v"
echo "</BODY>"
echo "</HTML>"
```

**Comenteu tots els problemes amb els que us heu trobat i les solucions que heu adoptat.**

*   No sabiem en quin format havíem de treure cgi (html o .txt)
*   Bona praxis executar el cgi al terminal abans de fer-ho al navegador
*   Cgi s’executa però no s’interpreta bé si en comptes de ser als directoris del servidor, estàs al sistema de fitxers locals


### Pregunta 2

**Incrusteu el contingut generat pel comptador en l'índex de la pràctica 1[ ](http://soft0.upc.edu/web/cgi/totext.cgi/dat/practica1/counter-page.html?UTF-8)**

Partint del cgi anterior, eliminem la part relativa al HTML

```bash
#!/bin/sh

echo "Content-Type: text/plain"
echo
v=$(cat visites.txt)
v=$(expr $v + 1)
echo $v > visites.txt
cat visites.txt
```

Cal tenir en compte que l’estructura del CGI ha estat:


    Content Type:text/plain
    echo
    (Nombre visites)
    echo

El contingut del cgi ha de passsar a ser Text Pla, no pas html. JS no processarà la sortida del CGI.

Ara doncs, sols cal incrustar el cgi a l’_index.html_ de la següent manera:

```html
<script type="text/javascript"
 src="/web/scripts/jquery-1.3.2.min.js"></script>
   <script type="text/javascript">
     $(document).ready(function(){
       $.get("comptadorJS.cgi", function(value){
         $(".counter").text(value);
       });
     });
   </script>
```
