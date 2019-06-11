# Pràctica 3: Realització d'un fòrum

# Índex
   * [Funcionalitats implementades](#funcionalitats-implementades)
      * [Consultar la llista de temes oberts sobre els que es poden fer qüestions i respostes.](#consultar-la-llista-de-temes-oberts-sobre-els-que-es-poden-fer-qüestions-i-respostes)
      * [Si l'usuari s'ha autentificat i és el webmaster, afegir nous temes.](#si-lusuari-sha-autentificat-i-és-el-webmaster-afegir-nous-temes)
      * [Veure les preguntes i respostes realitzades sobre un tema determinat.](#veure-les-preguntes-i-respostes-realitzades-sobre-un-tema-determinat)
         * [Preguntes](#preguntes)
         * [Respostes](#respostes)
      * [Afegir noves preguntes a un determinat tema](#afegir-noves-preguntes-a-un-determinat-tema)
      * [Afegir noves respostes a una determinada pregunta](#afegir-noves-respostes-a-una-determinada-pregunta)
   * [Modificacions de tema](#modificacions-de-tema)



Aquesta pràctica tracta del diseny i implementació d'un simple forum. El fòrum consta de **temes**, **preguntes** i **respostes**. Els administradors del fòrum poden crear temes, on, dins d'aquests, es crearan preguntes i respostes que podràn formular i respondre usuaris sense necessitat de ser administradors.

[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi) es troba el fòrum implementat.

# Funcionalitats implementades

## Consultar la llista de temes oberts sobre els que es poden fer qüestions i respostes.

Cada tema conté: un **títol**, un **_leader_** del tema i una **descripció** del tema. Amb el mètode `getHomeR` es llisten tots els temes oberts:

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
Un cop es cumpleixen aquestes dos premises, dins del mètode `postHomeR` es crea un formulari per fer _post_ d'un tema:

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

Aquí es mostra el Front End del formulari que crea:

![FormThemeScreenShot](/Practica3/project-p3/img/formThemes.png)


[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi/) online.  
_Cal estar registrat com administrador per poder veure-ho._


## Veure les preguntes i respostes realitzades sobre un tema determinat. 
Dins de cada tema, hi poden haver-hi vàries **preguntes**, i dins de cada pregunta, també poden haver-hi vàries **respostes**.

### Preguntes
Es mostren a través del mètode `getThemeR`. Les preguntes tenen com a atributs:

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

* L'**usuari** que ha fet la pregunta s'obté amb:
    ```haskell
    mbuser <- maybeAuthId
    ``` 
    
* La **data** en la que s'ha fet la pregunta, l'**assumpte**, i el **contingut** de la pregunta s'obtenen simplement fent un _get_ de la llista de respostes:
    ```haskell
    questions <- liftIO $ getQuestionList tid db
    ```
   
    Finalment, la implementació del mètode que mostra les diferents preguntes i respostes:
   
    ```haskell
    getThemeR :: ThemeId -> HandlerFor Forum Html
    getThemeR tid = do
    db <- getsSite forumDb
    mbuser <- maybeAuthId
    Just theme <- liftIO $ getTheme tid db
    questions <- liftIO $ getQuestionList tid db
    qformw <- generateAFormPost (questionForm tid)
    defaultLayout $(widgetTemplFile "src/forum/templates/currentTheme.html")
    ```
[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi/themes/1) un exemple del **FrontEnd**
   
   
### Respostes
Es mostren a través del mètode `getQuestionR`. Les respostes consten dels següents atributs:

```haskell
data Answer = Answer
        { aQuestion :: QuestionId
        , aUser :: UserId
        , aPosted :: UTCTime
        , aText :: Text
        }
        deriving (Show)
```

* L'**usuari** s'obté de la mateixa manera que amb les preguntes:
    ```haskell
    mbuser <- maybeAuthId
    ```
    
* La **data** i el **contingut**, s'obtenen simplement de fer un _get_ de la llista de respostes amb:
    ```haskell
    answers <- liftIO $ getAnswerList qid db
    ```
A continuació es mostra la implementació del mètode `getQuestionR`:

```haskell
getQuestionR :: ThemeId -> QuestionId -> HandlerFor Forum Html
getQuestionR tid qid = do
      db <- getsSite forumDb
      mbuser <- maybeAuthId
      Just theme <- liftIO $ getTheme tid db
      Just question <- liftIO $ getQuestion qid db
      answers <- liftIO $ getAnswerList qid db
      aformw <- generateAFormPost (answerForm tid)
      defaultLayout $(widgetTemplFile "src/forum/templates/currentQuestion.html")
```
[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi/themes/1/qs/1) un exemple del **Frontend**.


## Afegir noves preguntes a un determinat tema
Aquesta funcionalitat s'implementa en el mètode `postThemeR`.  

* El primer que s'ha de fer, seguint l'estructura ja vista, és comprovar que l'**usuari** s'ha autenticat:
    ```haskell
    user <- requireAuthId
    ```
    
* Després cal cear el **formulari** de preguntes.
    ```haskell
    (qformr, qformw) <- runAFormPost (questionForm tid)
    ```   
    Aquest formulari té el format:
    ```haskell
    questionForm :: ThemeId -> AForm (HandlerFor Forum) Question
    questionForm tid =
    Question <$> pure tid
           <*> liftToAForm requireAuthId --converteix accio del handler a un AForm --requireAuthId retorna autenticador o aborta
           <*> liftToAForm (liftIO getCurrentTime)
           <*> freq textField (withPlaceholder "Introduïu el títol de la pregunta" "Assumpte") Nothing
           <*> freq textareaField (withPlaceholder "Introduïu la descripció de la pregunta" "Descripció") Nothing
    ```

Aquí es mostra la impementació del mètode `postThemeR`

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
Aquesta funcionalitat s'implementa en el mètode `postQuestionR`. Per poder respondre a una pregunta, l'usuari s'ha d'haver identificat. Seguidament es crea el formulari de resposta, tot això seguint la mateixa estructura que per el POST de les preguntes i de les respostes.

El formulari de les respostes té la següent forma:

```haskell
answerForm :: QuestionId -> AForm (HandlerFor Forum) Answer
answerForm qid =
   Answer <$> pure qid
          <*> liftToAForm requireAuthId --converteix accio del handler a un AForm. requireAuthId retorna autenticador o aborta
          <*> liftToAForm (liftIO getCurrentTime)
          <*> freq textField (withPlaceholder "Introduïu la resposta" "Resposta") Nothing
```

A continuació es mostra el mètode `postQuestionR`. Bàsicament, és anàlog als mètodes POST mostrats anteriorment. Hi han parts del codi que s'explicaran més endavant (esborrar preguntes i respostes).

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

## Modificacions de tema
Per les modificacions s'ha decidit crear dos mètodes nous: `getThemeEditR` i `postThemeEditR`.
Si l'usuari s'ha autentificat i és el leader d'un determinat tema, aquest el pot modificar


 
   


   



   
   
   

 









