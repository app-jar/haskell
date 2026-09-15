module Main where

import Text.Printf
import Data.List (foldl', sortBy)

eps :: Double
eps = 1e-9

-- Расширенная матрица
newtype ExMatrix = ExMatrix [ExRow]

-- Строка расширенной матрицы
newtype ExRow = ExRow [Double]

instance Show ExRow where
    show (ExRow row) = "[" ++ show' row
        where
            show' []     = " ]"
            show' [x]    = " | " ++ printf "%6.2f" x ++      " ]"
            show' (x:xs) = "  "  ++ printf "%6.2f" x ++ show' xs

instance Show ExMatrix where
    show (ExMatrix rows) = unlines $ map show rows

-- Получение вектора свободных членов.
-- После преобразований - это вектор решений СЛАУ
extractVectorFree (ExMatrix rows) = map extractFree rows
extractFree (ExRow row) = last row

-- Проверяет, что строка нулевая
isDegenerate :: ExRow -> Bool
isDegenerate (ExRow xs) = all (\x -> abs x < eps) (init xs)

-- Умножение строки на скалаяр
(<**>) :: Double -> ExRow -> ExRow
(<**>) num (ExRow row) = ExRow $ map (num *) row

-- сложение/вычитание строк
(<+>) :: ExRow -> ExRow -> ExRow
(<+>) (ExRow row1) (ExRow row2) = ExRow $ zipWith (+) row1 row2
(<->) :: ExRow -> ExRow -> ExRow
(<->) (ExRow row1) (ExRow row2) = ExRow $ zipWith (-) row1 row2

-- Нормирует строку по первому ненулевому элементу
normByPivot :: ExRow -> ExRow
normByPivot (ExRow []) = ExRow []
normByPivot (ExRow (0:xs)) = ExRow (0: normed) where ExRow normed = normByPivot (ExRow xs)
normByPivot (ExRow (x:xs)) = (1 / x) <**> ExRow (x:xs)

-- Решение СЛАУ методом Гаусса
gauss :: ExMatrix -> Either String [Double]
gauss (ExMatrix matrix) = fmap extractVectorFree $ fmap ExMatrix $ gaussStep matrix
    where
        -- Прямой ход метода Гауса
        -- приведение матрицы к верхнетреугольной с единицами на галвной диагонали
        gaussStep :: [ExRow] -> Either String [ExRow]
        gaussStep [] = Right []
        gaussStep rows
            | isDegenerate solve = degenerateResult solve
            | otherwise          = checkErr solve gaussed
          where
            (solve:other)       = map normByPivot $ sortBy compareExRows rows
            gaussed             = gaussStep $ map (<-> solve) other
            checkErr s (Right gaussed') = Right $ gaussBackStep s gaussed' : gaussed'
            checkErr _ (Left e)  = Left e

        -- Обратный ход метода Гауса
        -- приведение матрицы к единичной
        gaussBackStep = foldl' gaussBackStep'
        gaussBackStep' target sub =
            let coeff = getCoef target sub
            in target <-> (coeff <**> sub)

        -------------

        -- Синхронно ищем коэффициент над ведущей единицей
        getCoef (ExRow target) (ExRow sub) = getCoef' target sub
        getCoef' (t:ts) (s:ss)
             | abs s < eps = getCoef' ts ss
             | otherwise   = t
        getCoef' _ _ = error "Несовпадение размеров строк"

        compareExRows (ExRow (x:_)) (ExRow (y:_)) = compare (abs y) (abs x)
        compareExRows _ _ = compare 0.0 0.0

        -- Возврат для нерешаемых систем
        degenerateResult :: ExRow -> Either String a
        degenerateResult (ExRow xs)
            | abs (last xs) < eps = Left "Бесконечно много решений"
            | otherwise           = Left "Система несовместна"



main :: IO ()
main = do
    -- Тестовая система уравнений:
    --  1x + 2y - 1z = 9
    --  2x - 1y + 3z = 13
    --  3x + 2y - 5z = -1
    let
        matrix = ExMatrix [ ExRow [1, 2, -1, 9]
                           , ExRow [2, -1, 3, 13]
                           , ExRow [3, 2, -5, -1]
                           ]
        smatr = show matrix
        result = gauss $ matrix
    case result of
        Left err -> putStrLn $ smatr ++ "\n\nОшибка: " ++ err
        Right xs -> putStrLn $ smatr ++ "\n\nРезультат (x, y, z): " ++ show xs
