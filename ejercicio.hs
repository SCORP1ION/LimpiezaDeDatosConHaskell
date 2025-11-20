{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Csv as Csv
import Data.Maybe (isNothing, fromMaybe)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Control.Monad (forM_)

-- Definimos el tipo de datos (equivalente a columnas del CSV)  
data Partido = Partido
  { fecha_partido   :: Maybe String
  , equipo_local    :: Maybe String
  , equipo_visitante:: Maybe String
  , goles_local     :: Maybe Double
  , goles_visitante :: Maybe Double
  } deriving (Show)

-- Instancias necesarias para CSV
instance Csv.FromNamedRecord Partido where
  parseNamedRecord m = Partido
    <$> m Csv..: "fecha_partido"
    <*> m Csv..: "equipo_local"
    <*> m Csv..: "equipo_visitante"
    <*> m Csv..: "goles_local"
    <*> m Csv..: "goles_visitante"

instance Csv.ToNamedRecord Partido where
  toNamedRecord p = Csv.namedRecord
    [ "fecha_partido"   Csv..= fecha_partido p
    , "equipo_local"    Csv..= equipo_local p
    , "equipo_visitante"Csv..= equipo_visitante p
    , "goles_local"     Csv..= goles_local p
    , "goles_visitante" Csv..= goles_visitante p
    ]

main :: IO ()
main = do
  putStrLn "Leyendo archivo partidos_cebollitas.csv..."
  csvData <- BL.readFile "partidos_cebollitas.csv"

  case Csv.decodeByName csvData of
    Left err -> putStrLn ("Error al leer CSV: " ++ err)
    Right (_, partidos) -> do
      let total = V.length partidos
      putStrLn $ "Total de filas leídas: " ++ show total

      -- Imputación: reemplazar nulos en goles_local por el promedio
      let promedio = promedioGoles partidos
      let partidosImputados = V.map (imputarGoles promedio) partidos

      putStrLn "\nValores nulos después de imputación:"
      print (contarNulos partidosImputados)

      -- Intentar convertir fechas
      let partidosFechas = V.map convertirFechas partidosImputados
      putStrLn "\nFechas inválidas (NaT):"
      print (contarFechasInvalidas partidosFechas)

      -- Mostrar primeras 5 filas
      putStrLn "\nPrimeras 5 filas:"
      forM_ (V.take 5 partidosFechas) print

      putStrLn "\nFin del procesamiento."

-- Función para calcular el promedio de goles_local
promedioGoles :: V.Vector Partido -> Double
promedioGoles ps =
  let vals = [ g | p <- V.toList ps, Just g <- [goles_local p] ]
  in if null vals then 0 else sum vals / fromIntegral (length vals)

-- Imputar valores nulos con el promedio
imputarGoles :: Double -> Partido -> Partido
imputarGoles prom p = p { goles_local = Just (fromMaybe prom (goles_local p)) }

-- Contar valores nulos
contarNulos :: V.Vector Partido -> Int
contarNulos = V.length . V.filter (isNothing . goles_local)

-- Convertir fecha a tipo Day (si es válida)
convertirFechas :: Partido -> Partido
convertirFechas p = p { fecha_partido = case fecha_partido p of
    Just f  -> case parseTimeM True defaultTimeLocale "%Y-%m-%d" f of
                 Just (_ :: Day) -> Just f
                 Nothing -> Nothing
    Nothing -> Nothing }

-- Contar fechas inválidas
contarFechasInvalidas :: V.Vector Partido -> Int
contarFechasInvalidas = V.length . V.filter (isNothing . fecha_partido)
