module Pages.Admin (component) where

import Prelude hiding (div)

import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, defaultEval, mkComponent, mkEval) as H
import Halogen.HTML (div, h1, h2, section, text)
import Ui.Utils (class_)

component :: forall q i. H.Component q i Void Aff
component =
  H.mkComponent
    { initialState: const unit
    , render: const render
    , eval: H.mkEval H.defaultEval
    }

render :: forall action slots. H.ComponentHTML action slots Aff
render =
  div [ class_ "row justify-content-center mt-4" ]
    [ div [ class_ "col-12 col-lg-8" ]
        [ div [ class_ "card shadow-sm border-0" ]
            [ div [ class_ "card-body p-4" ]
                [ h1 [ class_ "h3 mb-3" ] [ text "Administration utilisateurs" ]
                , div [ class_ "text-muted mb-4" ] [ text "Cet espace servira a gerer les comptes en attente et les utilisateurs existants." ]
                , section [ class_ "mb-4" ]
                    [ h2 [ class_ "h5 mb-2" ] [ text "Comptes en attente" ]
                    , div [ class_ "text-muted" ] [ text "La gestion des approbations sera ajoutee dans la prochaine story." ]
                    ]
                , section [ class_ "mb-1" ]
                    [ h2 [ class_ "h5 mb-2" ] [ text "Utilisateurs existants" ]
                    , div [ class_ "text-muted" ] [ text "La gestion des suppressions sera ajoutee dans une story dediee." ]
                    ]
                ]
            ]
        ]
    ]
