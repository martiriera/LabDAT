# Pràctica 3: Realització d'un fòrum

Aquesta pràctica tracta del diseny i implementació d'un simple forum. El fòrum consta de **temes**, **preguntes** i **respostes**. L'administrador del fòrum pot crear temes assignant un responsable, dins d'aquests temes, tots els usuaris identificats (admin inclòs) podràn formular preguntes i respondre-les.

[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi) es troba el fòrum implementat.

# Índex
   * [Funcionalitats implementades](#funcionalitats-implementades)
      * [Consultar la llista de temes oberts sobre els que es poden fer qüestions i respostes.](#consultar-la-llista-de-temes-oberts-sobre-els-que-es-poden-fer-qüestions-i-respostes)
      * [Si l'usuari s'ha autentificat i és el webmaster, afegir nous temes.](#si-lusuari-sha-autentificat-i-és-el-webmaster-afegir-nous-temes)
      * [Veure les preguntes i respostes realitzades sobre un tema determinat.](#veure-les-preguntes-i-respostes-realitzades-sobre-un-tema-determinat)
         * [Preguntes](#preguntes)
         * [Respostes](#respostes)
      * [Afegir noves preguntes a un determinat tema](#afegir-noves-preguntes-a-un-determinat-tema)
      * [Afegir noves respostes a una determinada pregunta](#afegir-noves-respostes-a-una-determinada-pregunta)
      * [Modificacions a un tema](#modificacions-a-un-tema)
      * [Eliminar continguts](#eliminar-continguts)


# Funcionalitats implementades

## Consultar la llista de temes oberts sobre els que es poden fer qüestions i respostes.

Cada tema conté un **títol**, un **leader** i una **descripció** del tema. Amb el mètode `getHomeR` (proveït pel professor) es generen diverses variables: les que permeten identificar la db o l'usuari, la que utilitzarem per llistar tots els temes creats i  també la que generarà un forumlari.

```haskell
getHomeR :: HandlerFor Forum Html
getHomeR = do
    -- Get model info
    db <- getsSite forumDb
    themes <- liftIO $ getThemeList db
    mbuser <- maybeAuthId
    tformw <- generateAFormPost (themeForm Nothing)
    defaultLayout $ $(widgetTemplFile "src/forum/templates/home.html")       
```

## Si l'usuari s'ha autentificat i és el webmaster, afegir nous temes.

Comprovem que l'**usuari** s'a autentificat amb:

```haskell
user <- requireAuthId
```
I que és **administrador** amb:

```haskell
when (not (isAdmin user)) (permissionDenied "L'usuari no és l'administrador")
```
Amb això ens assegurem de que cap usuari que no sigui administrador pugui crear un tema. No n'hi ha prou amb la línia de codi de la plantilla html que fa que no aparegui el formulari en cas de no ser admin.

Un cop es cumpleixen aquestes dues premises, dins del mètode `postHomeR` (proveït pel professor) s'executa un formulari per fer _post_ d'un tema:

```haskell
postHomeR :: HandlerFor Forum Html
postHomeR = do
    user <- requireAuthId
    db <- getsSite forumDb
    when (not (isAdmin user)) (permissionDenied "L'usuari no és l'administrador")
    (tformr, tformw) <- runAFormPost (themeForm Nothing)
    case tformr of
      -- S'ha de comprovar si el Form és success, missing o failure
        FormSuccess newtheme -> do
            liftIO $ addTheme newtheme db
            redirectRoute HomeR []
        _ -> do
            themes <- liftIO $ getThemeList db
            let mbuser = Just user
            defaultLayout $(widgetTemplFile "src/forum/templates/home.html")
```
És important fixar-se on redirigim a l'usuari quan fa submit dels formularis. Això es fa usant `redirectRoute`.

Aquest formulari té la següent estructura:

```haskell
themeForm :: Maybe Theme -> AForm (HandlerFor Forum) Theme
themeForm maybeth =
    Theme <$> freq (checkM checkUserExists textField)
                   (withPlaceholder "Introduiu el nom de l'usuari responsable" "Nom del responsable")
                   (tLeader <$> maybeth)
          <*> pure ""
          <*> freq textField (withPlaceholder "Introduiu el títol del tema" "Titol del tema") (tTitle <$> maybeth)
          <*> freq textareaField (withPlaceholder "Introduiu la descripció del tema" "Descripció") (tDescription <$> maybeth)
```
_Nota: Tal i com hem pensat la implementació per a modificar temes, s'ha hagut de variar una mica themeForm_

Aquí es mostra una captura del Front End del formulari que es genera:
![FormThemeScreenShot](Practica3/project-p3/img/formThemes.png)

Els mètodes de `HomeR` així com les variables definides s'utilitzen per conformar la plantilla [home.html](project-p3/src/forum/templates/home.html) que és la pàgina principal del fòrum. Amb l'ajuda de `getThemeList`, hem posat a la variable _themes_  una llista de temes formats per parelles (tid, theme). D'aquest segon element de cada tema s'en extreu el contingut.

[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi/) es pot veure el comportament de la pàgina online.  

## Veure les preguntes i respostes realitzades sobre un tema determinat. 
Dins de cada tema, hi poden haver-hi vàries **preguntes**, i dins de cada pregunta, també poden haver-hi vàries **respostes**.

### Preguntes
Cada pregunta té els següents atributs:

```haskell
data Question = Question
        { qTheme :: ThemeId
        , qUser :: UserId
        , qPosted :: UTCTime
        , qTitle :: Text
        , qText :: Text
        }
        deriving (Show)
```
Per llistar les preguntes d'un cert tema es segueix un procediment molt semblant al que hem utilitzat anteriorment.
Amb el mètode `getThemeR` obtenim principalment l'usuari i la llista de preguntes amb utilitzant `getQuestionList` així com altres variables que necessitarem per a la plantilla. 

```haskell
getThemeR :: ThemeId -> HandlerFor Forum Html
getThemeR tid = do
    -- fail "A completar per l'estudiant"
    db <- getsSite forumDb
    mbuser <- maybeAuthId
    -- let mbuser = Just user
    -- mbuser <- requireAuthId
    Just theme <- liftIO $ getTheme tid db
    questions <- liftIO $ getQuestionList tid db
    qformw <- generateAFormPost (questionForm tid)
    defaultLayout $(widgetTemplFile "src/forum/templates/currentTheme.html")
```

A la plantilla [currentTheme.html](project-p3/src/forum/templates/currentTheme.html) hi trobem bàsicament tres parts diferenciades. 
Primerament l'encarregada de mostrar la informació del tema en questió així com modificar-lo si es dóna el cas.
En segon lloc s'itera sobre la llista de preguntes mostrant-les totes, tal com hem fet amb els temes. Notar que entre els atributs de la pregunta hi ha qPosted, que ens diu quan s'ha fet la pregunta. El métode que ens dóna aquest temps `getCurrentTime`, retorna un IO UTCTime que posteriorment ha de ser convertit a Text amb `show`.
Per últim mostrem el formulari per crear pregunta.

[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi/themes/1) un exemple d'un tema al fòrum.
   
### Respostes
Per mostrar les respostes d'un determinat tema es fa pràcticament igual que com hem vist fins ara. A cada resposta hi trobem els següents atributs:

```haskell
data Answer = Answer
        { aQuestion :: QuestionId
        , aUser :: UserId
        , aPosted :: UTCTime
        , aText :: Text
        }
        deriving (Show)
```
El mètode `getQuestionR` és el següent:

```haskell
getQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
getQuestionR tid qid = do
      -- fail "A completar per l'estudiant"
      db <- getsSite forumDb
      mbuser <- maybeAuthId
      Just theme <- liftIO $ getTheme tid db
      Just question <- liftIO $ getQuestion qid db
      answers <- liftIO $ getAnswerList qid db
      aformw <- generateAFormPost (answerForm tid)
      defaultLayout $(widgetTemplFile "src/forum/templates/currentQuestion.html")
```

A la plantilla [currentQuestion.html](project-p3/src/forum/templates/currentQuestion.html) hi trobem bàsicament tres parts diferenciades. 
Primerament l'encarregada de mostrar la informació de la pregunta en questió així com eliminar-la si es dóna el cas. 
En segon lloc s'itera sobre la llista de respostes mostrant-les totes. També aquí es dóna l'opció d'esborrar una resposta determinada.
Per últim mostrem el formulari per crear respostes.

[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi/themes/1/qs/1) un exemple d'una pregunta determinada


## Afegir noves preguntes a un determinat tema
Amb `getThemeR` i `postThemeR` generem i executem el formulari que servirà per afegir preguntes al tema. Aquest té el format següent:

 ```haskell
    questionForm :: ThemeId -> AForm (HandlerFor Forum) Question
    questionForm tid =
    Question <$> pure tid
           <*> liftToAForm requireAuthId --converteix accio del handler a un AForm --requireAuthId retorna autenticador o aborta
           <*> liftToAForm (liftIO getCurrentTime)
           <*> freq textField (withPlaceholder "Introduïu el títol de la pregunta" "Assumpte") Nothing
           <*> freq textareaField (withPlaceholder "Introduïu la descripció de la pregunta" "Descripció") Nothing
```
El métode `postThemeR` és:

```haskell
postThemeR :: ThemeId -> HandlerFor Forum Html
postThemeR tid = do
  user <- requireAuthId
  db <- getsSite forumDb
  (qformr, qformw) <- runAFormPost (questionForm tid)
  Just theme <- liftIO $ getTheme tid db
  case qformr of
      FormSuccess newquestion -> do
          liftIO $ addQuestion newquestion db
          redirectRoute (ThemeR tid) []
      _ -> do
          questions <- liftIO $ getQuestionList tid db
          let mbuser = Just user
          defaultLayout $(widgetTemplFile "src/forum/templates/currentTheme.html")
```

## Afegir noves respostes a una determinada pregunta
De manera anàloga a l'anterior punt, generem i executem un formulari que té els següests atributs. La part del codi relacionada amb la eliminació de continguts s'explicarà més endavant: 

```haskell
answerForm :: QuestionId -> AForm (HandlerFor Forum) Answer
answerForm qid =
   Answer <$> pure qid
          <*> liftToAForm requireAuthId --converteix accio del handler a un AForm. requireAuthId retorna autenticador o aborta
          <*> liftToAForm (liftIO getCurrentTime)
          <*> freq textField (withPlaceholder "Introduïu la resposta" "Resposta") Nothing
```

A continuació es mostra el mètode `postQuestionR`:

```haskell
postQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
postQuestionR tid qid = do
  db <- getsSite forumDb
  user <- requireAuthId
  Just theme <- liftIO $ getTheme tid db
  Just question <- liftIO $ getQuestion qid db
  answers <- liftIO $ getAnswerList qid db
  isDeleteQuestion <- isJust <$> lookupPostParam "delete-question"
  isDeleteAnswer <- isJust <$> lookupPostParam "delete-answer"
  if isDeleteQuestion then do
    liftIO $ deleteQuestion qid db
    liftIO $ forM_ answers $ \ (aid,_) -> deleteAnswer aid db
    redirectRoute (ThemeR tid) []
  else if isDeleteAnswer then do
    Just textaid <- lookupPostParam "aid"
    let Just aid = fromPathPiece textaid
    liftIO $ deleteAnswer aid db
    redirectRoute (QuestionR tid qid) []
  else do
    (aformr, aformw) <- runAFormPost (answerForm qid)
    case aformr of
        FormSuccess newanswer -> do
            liftIO $ addAnswer newanswer db
            redirectRoute (QuestionR tid qid) []
        _ -> do
            questions <- liftIO $ getAnswerList qid db
            let mbuser = Just user
            defaultLayout $(widgetTemplFile "src/forum/templates/currentQuestion.html")
```

## Modificacions a un tema
L'objectiu era que un usuari autentificat com a leader d'un tema pugi editar-ne el contingut. Per comoditat, hem pensat que el millor seria generar un formulari semblant al de crear tema a una nova plantilla html.

Primerament s'ha creat una nova ruta anomenada `themeEditR` al fitxer [App.hs](project-p3/src/forum/haskell/App.hs) que conté els també nous métodes `getThemeEditR` i `postThemeEditR`. Aquests són molt semblants als de la creació de temes:
```haskell
getThemeEditR :: ThemeId -> HandlerFor Forum Html
getThemeEditR tid = do
    db <- getsSite forumDb
    Just theme <- liftIO $ getTheme tid db
    user <- requireAuthId
    when (not (isLeader theme user)) (permissionDenied "L'usuari no és el moderador")
    tformw <- generateAFormPost (themeForm (Just theme))
    defaultLayout $ $(widgetTemplFile "src/forum/templates/updateTheme.html")
```
Aquí veiem que per comprovar que l'usuari autentificat sigui el leader del tema, s'utilitza la funció `isLeader` que hem hagut de implementar a [Found.hs](project-p3/src/forum/haskell/Found.hs). 
Aquesta funció espera un tema i una id d'usuari. Si el leader del tema i aquesta id coincideixen retorna True. False en cas contrari:

```haskell
isLeader :: Theme -> UserId -> Bool
isLeader t u = (u == tLeader t)
```

```haskell
postThemeEditR :: ThemeId -> HandlerFor Forum Html
postThemeEditR tid = do
  user <- requireAuthId
  db <- getsSite forumDb
  Just theme <- liftIO $ getTheme tid db
  (tformr, tformw) <- runAFormPost (themeForm (Just theme))
  case tformr of
      FormSuccess edittheme -> do
          liftIO $ updateTheme tid edittheme db
          redirectRoute (ThemeR tid) []
      _ -> do
          themes <- liftIO $ getThemeList db
          let mbuser = Just user
          defaultLayout $(widgetTemplFile "src/forum/templates/updateTheme.html")
```

En aquest métode veiem que s'utilitza la funció updateTheme que modifica una entrada a la base de dades. Un problema que vàrem tenir és que el formulari que generàvem, com és lògic, estava buit. El que necessitavem eren els valors dels atributs que hi havien en el tema a modificar. És per això que vàrem haver de modificar lleugerament themeForm. 

Sols ha calgut que el primer atribut de la funció sigui de tipus Maybe Theme en comptes de sols Theme. I hem fet que els valors per defecte fossin `tLeader` `tTitle` i `tDescription` mapejats al context Maybe del tema. D'aquesta manera, quan a `getThemeEditR` i `postThemeEditR` invoquem ```themeForm (Just theme)``` els camps del formulari de tema s'omplen amb els valors que hi havia a `theme`. En canvi quan ho hem usat per crear un nou formulari hem fet ```themeForm (Just Nothing)``` perquè els camps estiguéssin buits.

Un cop implementat això, sols ha calgut crear un enllaç a _currentTheme.html_ "protegit" amb isLeader que ens porti a la plantilla [updateTheme.html](project-p3/src/forum/templates/updateTheme.html). Aquesta sols conté el form per modificar el tema.

  


   



   
   
   

 









