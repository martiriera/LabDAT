# Pràctica 3: Realització d'un fòrum

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
Aquí es mostra el Front End del formulari que crea:
![FormThemeScreenShot](/Practica3/project-p3/img/formThemes.png)

[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi/) online.  
_Cal estar registrat com administrador per poder veure-ho._

## Veure les preguntes i respostes realitzades sobre un tema determinat. 
Dins de cada tema, hi poden haver-hi vàries **preguntes**, i dins de cada pregunta, també poden haver-hi vàries **respostes**.

Pel que fa a les **preguntes**, es mostren a través del mètode `getThemeR`. Les preguntes tenen com a atributs:
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
   * La **data** en la que s'ha fet la pregunta, l'**assumpte**, i el **contingut** de la pregunta s'obtenen amb:
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
   
 









