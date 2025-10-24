module BellLaPadula where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (mapMaybe)

-- Tipos básicos
data SecurityLevel = Unclassified | Confidential 
                  | Secret | TopSecret 
                  deriving (Eq, Ord, Show)

data AccessType = Read | Write deriving (Eq, Show)

type UserName = String
type FileName = String

data User = User
  { userName :: UserName
  , userClearance :: SecurityLevel
  } deriving (Show)

data File = File
  { fileName :: FileName
  , fileClassification :: SecurityLevel
  } deriving (Show)

-- Control de acceso básico
grant :: User -> File -> AccessType -> Bool
grant user file access = case access of
  Read  -> userClearance user >= fileClassification file
  Write -> userClearance user <= fileClassification file

----- Extensión: Asegurar propiedad *

-- Estado del sistema
type OpenFiles = Map (UserName, FileName) AccessType

data SystemState = SystemState
  { files :: Map FileName File            -- Store all files in the system
  , openFiles :: OpenFiles
  } deriving (Show)

-- Archivos abiertos en modo lectura por un usuario
openReadFiles :: User -> SystemState -> [File]
openReadFiles user state =
    mapMaybe (\((uname,fname), mode) ->
                if uname == userName user && mode == Read
                then Map.lookup fname (files state)
                else Nothing)
             (Map.toList $ openFiles state)

-- Control de acceso con seguimiento del estado del sistema
grant' :: User -> File -> AccessType -> SystemState -> (Bool, SystemState)
grant' user file access state =
  let canAccess = grant user file access

      -- Maximo nivel de seguridad en archivos abiertos
      maxReadLevel = case openReadFiles user state of
                       [] -> fileClassification file
                       fs -> maximum $ map fileClassification fs

      propertyStarViolation = case access of
        Write -> fileClassification file < maxReadLevel
        _     -> False

      accessGranted = canAccess && not propertyStarViolation

      -- Actualizar el estado del sistema
      newOpenFiles = if accessGranted
                     then Map.insert (userName user, fileName file)
                                    access (openFiles state)
                     else openFiles state
  in
  (accessGranted, state { openFiles = newOpenFiles })

-- Cerrar archivos
closeFile :: User -> File -> SystemState -> SystemState
closeFile user file state =
  state { openFiles = Map.delete (userName user, fileName file) (openFiles state) }
