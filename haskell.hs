import Data.Char (toLower)
import Data.List (lookup)

-- Chave de substituição para o ciframento
chaveCifra :: String
chaveCifra = "ZYNGWQAMXPKVULCEFRIBSJDOTH"

-- Remove acentos e converte para minúscula
removerAcento :: Char -> Char
removerAcento c =
  case toLower c of
    'á' -> 'a'; 
    'à' -> 'a'; 
    'ã' -> 'a'; 
    'â' -> 'a'
    'é' -> 'e'; 
    'ê' -> 'e'
    'í' -> 'i'; 
    'î' -> 'i'
    'ó' -> 'o'; 
    'õ' -> 'o'; 
    'ô' -> 'o'
    'ú' -> 'u'; 
    'û' -> 'u'
    _   -> c

-- Normaliza caracteres para processamento
normalizar :: Char -> Char
normalizar c = toLower (removerAcento c)

-- Ciframento monalfabético
monoAlphaCipherE :: String -> String -> String
monoAlphaCipherE chave texto = map cifrarChar texto
  where
    alfabeto = ['a'..'z']
    chaveMinuscula = map toLower chave
    tabelaSubstituicao = zip alfabeto chaveMinuscula

    cifrarChar c =
      case lookup (normalizar c) tabelaSubstituicao of
        Just valor -> valor
        Nothing    -> c

-- Deciframento monalfabético
monoAlphaCipherD :: String -> String -> String
monoAlphaCipherD chave texto = map decifrarChar texto
  where
    alfabeto = ['a'..'z']
    chaveMinuscula = map toLower chave
    tabelaDeciframento = zip chaveMinuscula alfabeto

    decifrarChar c =
      case lookup (normalizar c) tabelaDeciframento of
        Just valor -> valor
        Nothing    -> c


criptografar :: String -> String
criptografar texto = monoAlphaCipherE chaveCifra texto


descriptografar :: String -> String
descriptografar texto = monoAlphaCipherD chaveCifra texto