
{-# LANGUAGE OverloadedStrings #-}

module App_prev
where
import           Calc
import           WaiUtils
import           Html
import           TextUtils

import           Network.Wai
import           Network.HTTP.Types

import           Data.Complex
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad
import           Control.Applicative

-- Versio que no usa el monad 'Handler'.
-- Resulta una mica mes complicada, usant directament funcions del WAI,
-- pero mostra la funcionalitat que haura de tenir la versio de l'estudiant
-- (concretament, l'estudiant haura de definir la funcio 'doPost' usant el monad 'Handler').

-- ****************************************************************
-- WAI application

calcApp :: Application
calcApp req respond = do
    let meth = requestMethod req
    if meth == methodGet then do
        doGet req respond
    else if meth == methodPost then do
        doPost req respond
    else
        respond $ errorResponse methodNotAllowed405 $ "Invalid request method " <> showt meth

-- ****************************************************************

-- Executed when the HTTP method is GET
doGet :: Application
doGet req respond = do
    let mbcalc = lookup "calcState" (requestSession req) >>= readt
        calc = maybe calcInit id mbcalc
        (_, hpanel) = runForms Nothing
        html = pageHtml hpanel calc Nothing
    respond $ htmlResponse (runHtmlToText html) [("calcState", showt calc)]

-- Executed when the HTTP method is POST
doPost :: Application
doPost req respond = do
    q <- requestGetPostQuery req
    let mbcalc = lookup "calcState" (requestSession req) >>= readt
        calc = maybe calcInit id mbcalc
        (mbev, hpanel) = runForms (Just q)
    case mbev of
        Just ev ->
            case calcSolve1 ev calc of
                Right calc2 -> do
                    respond $ redirectResponse "#" [("calcState", showt calc2)]
                Left err -> do
                    let html = pageHtml hpanel calc (Just err)
                    respond $ htmlResponse (runHtmlToText html) [("calcState", showt calc)]
        Nothing -> do
            let html = pageHtml hpanel calc Nothing
            respond $ htmlResponse (runHtmlToText html) [("calcState", showt calc)]

-- ****************************************************************
-- Tractament dels formularis corresponents a les diferents operacions (events)
-- de la calculadora.

type CalcNumber = Complex Double

buttons :: [[(CalcEvent CalcNumber, Text)]]
buttons = [ [ (CalcBin (+), "x1 + x0"), (CalcBin (-), "x1 - x0"), (CalcBin (*), "x1 * x0"), (CalcBin (/), "x1 / x0") ]
          , [ (CalcUn ((0:+1)*), "j* x0"), (CalcUn negate, "-x0"), (CalcUn (1/), "1/x0"), (CalcUn conjugate, "conj x0") ]
          , [ (CalcUn ((:+0) . realPart), "real x0"), (CalcUn ((:+0) . imagPart), "imag x0")
            , (CalcUn ((:+0) . magnitude), "mod x0"), (CalcUn ((:+0) . phase), "arg x0") ]
          , [ (CalcDup, "..,x0 -> ..,x0,x0"), (CalcPop, "..,x0 -> .."), (CalcFlip, "..,x1,x0 -> ..,x0,x1") ]
          ]

runForms :: Maybe Query -> (Maybe (CalcEvent CalcNumber), Html)
runForms mbq =
    let (mbeev, html1) = runEnterForm mbq
        (mbbev, html2) = runButtonPanel buttons mbq
    in ( (CalcEnter <$> mbeev) <|> mbbev, html1 <> html2 )

runEnterForm :: Maybe Query -> (Maybe CalcNumber, Html)
runEnterForm mbq =
    let mbq' = do
                q <- mbq
                const q <$> lookupParam "enter" q
        (mbr, htmlr) = runDoubleField "real" "Part real" mbq'
        (mbi, htmli) = runDoubleField "imag" "Part imaginària" mbq'
    in ( (:+) <$> mbr <*> mbi
       , hElem "form" [ ("method", "POST"), ("action", "#") ] $
             hElem "div" [ ("class", "form-row") ] $ do
                hElem "div" [ ("class", "col-md-5") ] htmlr
                hElem "div" [ ("class", "col") ] $
                    hElem "span" [ ("class", "form-control") ] $ hText " + j * "
                hElem "div" [ ("class", "col-md-5") ] htmli
                hElem "div" [ ("class", "col-md-1") ] $
                    hElem "button" [ ("type", "submit"), ("class", "btn btn-info"), ("name", "enter") ] $ hText "Enter"
       )

runDoubleField :: Text -> Text -> Maybe Query -> (Maybe Double, Html)
runDoubleField name ph mbq =
    let (res, val, mberr) = case mbq of
            Nothing -> (Nothing, "0.0", Nothing)
            Just q ->
                let t = maybe "" id $ lookupParam name q
                in if T.null t
                    then (Nothing, t, Just "Valor requerit")
                    else case readtEither t of
                        Left err -> (Nothing, t, Just err)
                        Right d -> (Just d, t, Nothing)
        ident = "entry." <> name
        hgroup = case mberr of
            Just err -> hElem "div" [ ("class", "form-group"), ("optional",""), ("has-error","") ] $ do
                            hElemEmpty "input" [ ("type", "text"), ("class", "form-control is-invalid")
                                               , ("id", ident), ("name", name), ("value", val), ("placeholder", ph) ]
                            hElem "div" [ ("class", "invalid-feedback") ] $ do
                                hElem "span" [ ("class", "form-text text-muted") ] $
                                    hText err
            Nothing  -> hElem "div" [ ("class", "form-group"), ("optional","") ] $ do
                            hElemEmpty "input" [ ("type", "text"), ("class", "form-control")
                                               , ("id", ident), ("name", name), ("value", val), ("placeholder", ph) ]
    in ( res, hElem "label" [ ("class","sr-only"), ("for", ident) ] (hText name)
               <> hgroup )

runButtonPanel :: [[(CalcEvent CalcNumber, Text)]] -> Maybe Query -> (Maybe (CalcEvent CalcNumber), Html)
runButtonPanel buttonss mbq =
    let (res, htmls) = unzip $ zipWith go [0 ..] buttonss
    in ( msum res , hElem "div" [ ("class", "btn-toolbar") ] $ mconcat htmls )
    where
        go row butts = runButtonRow row butts mbq

runButtonRow :: Int -> [(CalcEvent CalcNumber, Text)] -> Maybe Query -> (Maybe (CalcEvent CalcNumber), Html)
runButtonRow row buttons mbq =
    let (res, htmls) = unzip $ zipWith go [0 ..] buttons
    in ( msum res, hElem "div" [ ("class", "btn-group btn-group-lg") ] $ foldMap id htmls )
    where
        go col butt = runButtonForm row col butt mbq
        hcol = hElem "div" [ ("class", "col") ]

runButtonForm :: Int -> Int -> (CalcEvent CalcNumber, Text) -> Maybe Query -> (Maybe (CalcEvent CalcNumber), Html)
runButtonForm row col (event, label) mbq =
    let name = "butt-" <> showt row <> "-" <> showt col
        res = do
                q <- mbq
                const event <$> lookupParam name q
        hbutt = hElem "button" [ ("type", "submit"), ("class", "btn btn-info"), ("name", name) ] $ hText label
    in ( res, hElem "div" [ ("xxclass", "btn-group btn-group-lg") ] $
                  hElem "form" [ ("method", "POST"), ("action", "#") ] hbutt )

-- ****************************************************************
-- View

pageHtml :: Html -> CalcState CalcNumber -> Maybe Text -> Html
pageHtml hpanel calc mberror = do
    hDOCTYPE
    hElem "html" [] $ do
        hElem "head" [] $ do
            hElemEmpty "meta" [ ("charset", "UTF-8") ]
            hElem "title" [] $ hText "Calculadora"
            ---hElemEmpty "link" [ ("rel", "stylesheet"), ("href", "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css") ]
            hElemEmpty "link" [ ("rel", "stylesheet")
                              , ("href", "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")
                              , ("integrity", "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T")
                              , ("crossorigin", "anonymous") ]
        hElem "body" [] $ do
            hElem "div" [ ("class", "container-fluid") ] $ do
                hElem "h1" [] $ hText "Calculadora"
                hElemEmpty "hr" []
                hElem "h3" [] $ hText "Estat de la pila:"
                hElem "div" [ ("class", "panel scrollable") ] $
                    hElem "ul" [ ("class", "list-group list-group-flush") ] $
                        foldMap calcElem (reverse $ zip [0..] calc)
                hpanel
                case mberror of
                    Just err -> hElem "div" [ ("class", "message error") ] $ hText err
                    Nothing -> mempty
    where
        calcElem (i, num) = hElem "li" [ ("class", "list-group-item") ] $ do
                                hElem "span" [ ("class", "badge badge-pill badge-secondary") ] $ hText ("x" <> showt i)
                                hText (showNum num)
        showNum (re :+ im) = (if re /= 0.0 || im == 0.0 then showt re else "")
                              <> (if im < 0.0 then " - j * " <> showt (-im)
                                  else if im > 0.0 then " + j * " <> showt im
                                  else "")

