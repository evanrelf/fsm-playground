module Workflow
  ( Workflow
  , module Workflow.Concrete
  , module Workflow.Abstract
  , module Workflow.Abstract.TH
  , module Workflow.Info
  )
where

import Workflow.Abstract
import Workflow.Abstract.TH
import Workflow.Concrete
import Workflow.Info

type Workflow w = (AbstractWorkflow w, ConcreteWorkflow w)
