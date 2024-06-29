{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Graph
  ( dot
  )
where

import Info

import Algebra.Graph.Class
import Algebra.Graph.Export.Dot
import Algebra.Graph.ToGraph
import Data.List (find)
import Data.String (IsString (..))

instance ToGraph WorkflowInfo where
  type ToVertex WorkflowInfo = String
  toGraph info =
    overlays (map toGraph info.states ++ map toGraph info.transitions)

instance ToGraph StateInfo where
  type ToVertex StateInfo = String
  toGraph info = vertex info.name

instance ToGraph TransitionInfo where
  type ToVertex TransitionInfo = String
  toGraph info =
    connect (maybe (vertex "()") toGraph info.input) (toGraph info.output)

dot :: IsString s => WorkflowInfo -> s
dot info = fromString (export style info)
  where
  style =
    Style
      { graphName = info.name
      , preamble = map ("  // " ++) (lines info.description)
      , graphAttributes =
          [ "label" := info.name
          , "labelloc" := "top"
          ]
      , defaultVertexAttributes = []
      , defaultEdgeAttributes = []
      , vertexName = id
      , vertexAttributes = \name ->
          case findState name of
            Nothing | name == "()" ->
              [ "shape" := "point"
              , "label" := ""
              ]
            Nothing -> []
            Just _state -> []
      , edgeAttributes = \inputName outputName ->
          case findTransition inputName outputName of
            Nothing -> []
            Just transition -> ["label" := transition.name]
      , attributeQuoting = DoubleQuotes
      }

  findState name = find (\state -> state.name == name) info.states

  findTransition inputName outputName =
    find
      (\transition ->
          (maybe "()" (.name) transition.input == inputName)
       && (transition.output.name == outputName)
      )
      info.transitions
