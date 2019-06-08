# Pràctica 3: Realització d'un fòrum

Aquesta pràctica tracta del diseny i implementació d'un simple forum. El fòrum consta de **temes**, **preguntes** i **respostes**. Els administradors del fòrum poden crear temes, on, dins d'aquests, es crearan preguntes i respostes que podràn formular i respondre usuaris sense necessitat de ser administradors.

[Aquí](http://soft0.upc.edu/~ldatusr14/practica3/forum.cgi) es troba el fòrum implementat.

# Funcionalitats implementades

## Consultar la llista de temes oberts sobre els que es poden fer qüestions i respostes.

Amb aquest mètode es llisten tots els temes oberts. Cada tema conté:

* El **títol**.
* L'usuari que manté el tema: el **leader** del tema.
* **Descripció** del tema.





```
getThemeR :: ThemeId -> HandlerFor Forum Html
getThemeR tid = do    
    db <- getsSite forumDb
    mbuser <- maybeAuthId    
    Just theme <- liftIO $ getTheme tid db
    questions <- liftIO $ getQuestionList tid db
    qformw <- generateAFormPost (questionForm tid)
    defaultLayout $(widgetTemplFile "src/forum/templates/currentTheme.html")   
```
