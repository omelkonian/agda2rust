-- | Re-export things from the Agda library.
module Agda.Lib
  ( module Agda.Syntax.Position
  , module Agda.Syntax.Common
  , module Agda.Syntax.TopLevelModuleName
  , module Agda.Syntax.Abstract.Name
  , module Agda.Syntax.Internal
  , module Agda.Syntax.Literal
  , module Agda.Syntax.Translation.InternalToAbstract
  , module Agda.Compiler.ToTreeless
  , module Agda.ToTreeless
  , module Agda.Syntax.Treeless
  , module Agda.Compiler.Treeless.Pretty
  , module Agda.Compiler.Treeless.EliminateLiteralPatterns
  , module Agda.TypeChecking.Monad
  , module Agda.TypeChecking.Free
  , module Agda.TypeChecking.Datatypes
  , module Agda.TypeChecking.Records
  , module Agda.TypeChecking.Level
  , module Agda.TypeChecking.Substitute
  , module Agda.TypeChecking.Telescope
  , module Agda.TypeChecking.Primitive
  , module Agda.TypeChecking.Reduce
  , module Agda.Main
  , module Agda.Compiler.Common
  , module Agda.Compiler.Backend
  , module Agda.Syntax.Common.Pretty
  , module Agda.TypeChecking.Pretty
  , module Text.Show.Pretty
  , module Agda.Utils.Monad
  , module Agda.Utils.Maybe
  , module Agda.Utils.List
  ) where

-- * common syntax
import Agda.Syntax.Position
  ( Range(..), rStart, posLine )
import Agda.Syntax.Common
  ( Arg, unArg
  , ArgName, bareNameWithDefault
  , LensQuantity(..), hasQuantity0
  , LensHiding(..), visible
  , Ranged(..), Origin(..), getOrigin )
import Agda.Syntax.TopLevelModuleName
  ( TopLevelModuleName, moduleNameToFileName )

-- * abstract syntax
import Agda.Syntax.Abstract.Name
  ( qnameToList0 )

-- * internal syntax
import Agda.Syntax.Internal
  ( QName, qnameName, qnameModule, qnameFromList
  , Term(..), Type, Type''(El), unEl
  , Sort(..), Sort'(..), isSort
  , Abs(..), unAbs, absName
  , Dom(..), unDom, domName, pDom, defaultDom
  , Elim, argsFromElims
  , Telescope, Tele(..), ListTel, telToList, telFromList
  , ConHead(..)
  , Clause(..)
  , nameId, dbPatVarIndex )
import Agda.Syntax.Literal
  ( Literal(..) )
import Agda.Syntax.Translation.InternalToAbstract
  ( NamedClause(..) )

-- * treeless syntax
import Agda.Compiler.ToTreeless
  hiding ( toTreeless )
import Agda.ToTreeless
  ( toTreeless )
import Agda.Syntax.Treeless
  ( TTerm(..), TPrim(..), TAlt(..)
  , CaseInfo(..), CaseType(..)
  , EvaluationStrategy(..), isPrimEq
  , mkTLam, mkTApp, tLamView )
import Agda.Compiler.Treeless.Pretty ()
import Agda.Compiler.Treeless.EliminateLiteralPatterns
  ( eliminateLiteralPatterns )

-- * typechecking
import Agda.TypeChecking.Monad
  ( TCM, MonadTCM(liftTCM), MonadTCEnv, MonadReduce
  , PureTCM, ReadTCState, HasConstInfo, MonadAddContext
  , HasBuiltins, BuiltinId, getBuiltinName'
  , typeOfConst, getConstInfo, instantiateDef
  , getContext, addContext
  , reportSLn, VerboseLevel
  , Definition(..), Defn(..)
  , pattern Function, funProjection, funClauses, funWith, funExtLam
  , Projection(..)
  , pattern Datatype, dataCons, dataPars
  , pattern Constructor
  , pattern Record, recConHead, recPars, recTel
  , pattern Axiom, pattern DataOrRecSig
  , pattern Primitive, pattern PrimitiveSort
  , withCurrentModule, iInsideScope, setScope
  , CompilerPragma(..), getUniqueCompilerPragma )
import Agda.TypeChecking.Free
  ( freeVars, VarCounts(..) )
import Agda.TypeChecking.Datatypes
  ( getConstructorData, getConHead )
import Agda.TypeChecking.Records
  ( isRecord, isRecordConstructor, isRecordType )
import Agda.TypeChecking.Level
  ( isLevelType )
import Agda.TypeChecking.Substitute
  ( TelV(..), raise )
import Agda.TypeChecking.Telescope
  ( telViewPath, telViewUpTo, telView )
import Agda.TypeChecking.Primitive
  ( isBuiltin )
import Agda.TypeChecking.Reduce
  ( reduce )

-- * backends
import Agda.Main
  ( runAgda )
import Agda.Compiler.Common
  ( curIF, compileDir )
import Agda.Compiler.Backend
  ( Backend(..), Backend'(..), Recompile(..), IsMain, nameBindingSite
  , iForeignCode, getForeignCodeStack, ForeignCode(..)
  , Flag )

-- * pretty-printing
import Agda.Syntax.Common.Pretty
  ( Pretty, prettyShow, renderStyle, Style(..), Mode(..) )
import Agda.TypeChecking.Pretty
  ( PrettyTCM(..), MonadPretty, fsep, punctuate, braces, parens, Doc )
import Agda.TypeChecking.Pretty
  hiding (text)
import Text.Show.Pretty
  ( ppShow )

-- * Agda utilities
import Agda.Utils.Monad
  ( ifM, mapMaybeM, partitionM, ifM )
import Agda.Utils.Maybe
  ( ifJustM, boolToMaybe )
import Agda.Utils.List
  ( downFrom )
