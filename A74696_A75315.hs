--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 3 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também indentificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: A74696
-- Nome: José Diogo Paiva Bastos
-- Curso: MIEI
--
-- Aluno 2
-- Número: A75315
-- Nome: Ricardo Jorge Barroso Certo
-- Curso: MIEI
--




module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set
import Control.Monad.State
import Data.Maybe


--
-- Teste unitário
--
{-|g0 é um grafo vazio
-}
g0 :: Graph Int
g0 = Graph {nodes = fromList [],
            edges = fromList []
           }
{-|g1 é um grafo que contem apenas um vértice.
-}
g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }
{-|g2 é um grafo válido, acíclico, que é uma floresta e que forma uma linha entre os vértices 1 e 3.
-}
g2 :: Graph Int
g2 = Graph {nodes = fromList[1,2,3],edges = fromList[ Edge 1 2, Edge 2 3]}
{-|g3 é um grafo cíclico entre os vértices 1 e 3.
-}
g3 :: Graph Int
g3 = Graph {nodes = fromList[1,2,3],edges = fromList[Edge 1 2,Edge 2 3,Edge 3 1]}
{-|g4 não é um grafo válido pois contem o nodo 3 que não faz parte da lista dos vértices.
-}
g4 :: Graph Int
g4 = Graph {nodes = fromList[1,2],edges = fromList[ Edge 1 3]}
{-|g5 é um grafo válido cíclico entre os vértices 1 e 2.
-}
g5 :: Graph Int
g5 = Graph {nodes = fromList[1,2],edges = fromList[ Edge 1 2, Edge 2 1]}
{-|g6 é um subgrafo de g3.
-}
g6 :: Graph Int
g6 = Graph {nodes = fromList[1,2],edges = fromList[Edge 1 2]}
{-|g7 é o grafo invertido de g3.
-}
g7 :: Graph Int
g7 = Graph {nodes = fromList[1,2,3],edges = fromList[Edge 2 1,Edge 3 2,Edge 1 3]}
{-|g6 é um subgrafo de g9.
-}
g8 :: Graph Int
g8 = Graph {nodes = fromList[3,4],edges = fromList[Edge 4 3]}
{-|g9 é a união de g2 com g8.
-}
g9 :: Graph Int
g9 = Graph {nodes = fromList[1,2,3,4],edges = fromList[ Edge 1 2, Edge 2 3,Edge 4 3]}
{-|g10 é um grafo com 1 vértice de grau 0 e 3 de grau 1.
-}
g10 :: Graph Int
g10 = Graph {nodes = fromList[1,2,3,4],edges = fromList[Edge 1 2,Edge 1 4,Edge 1 3]}
{-|g11 é um grafo com 1 vértice de grau 0 e 2 de grau 1 e 2 de grau 2.
-}
g11 :: Graph Int
g11 = Graph {nodes = fromList[1,2,3,4,5],edges = fromList[Edge 1 2,Edge 1 3,Edge 2 5,Edge 3 4]}
{-|g12 é um grafo igual ao g11 mas com a adição de uma aresta.
-}
g12 :: Graph Int
g12 = Graph {nodes = fromList[1,2,3,4,5],edges = fromList[Edge 1 2,Edge 1 3,Edge 2 5,Edge 3 4,Edge 4 5]}

-- Um exemplo de um teste unitário.
--test_adj :: Test
--test_adj = adj g1 1 ~?= fromList [Edge 1 1]

--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--
{-| Testes unitários para a função swap
-}
t1 = swap (Edge 2 3) ~?= (Edge 3 2)
t2 = swap (Edge 1 1) ~?= (Edge 1 1)
{-| Teste unitário para a função empty
-}
t3 = g0 ~?= Graph.empty
{-| Testes unitários para a função isEmpty
-}
t4 = isEmpty g0 ~?= True
t5 = isEmpty g2 ~?= False
{-| Testes unitários para a função isValid
-}
t6 = isValid g4 ~?= False 
t7 = isValid g3 ~?= True
t8 = isValid g2 ~?= True
t9 = isValid g5 ~?= True
t10 = isValid g0 ~?= True
{-| Testes unitários para a função isDAG
-}
t11 = isDAG g3 ~?= False
t12 = isDAG g2 ~?= True
t13 = isDAG g12 ~?= True
t14 = isDAG g5 ~?= False
t15 = isDAG g1 ~?= False
{-| Testes unitários para a função isForest
-}
t16 = isForest g9 ~?= True
t17 = isForest g2 ~?= True 
t18 = isForest g11 ~?= False 
{-| Testes unitários para a função isSubgraphOf
-}
t19 = isSubgraphOf g1 g3 ~?= False
t20 = isSubgraphOf g3 g2 ~?= False
t21 = isSubgraphOf g6 g3 ~?= True 
t22 = isSubgraphOf g0 g1 ~?= True
{-| Testes unitários para a função adj
-}
t23 = adj g2 1 ~?= fromList [Edge 1 2]
t24 = adj g6 1 ~?= fromList [Edge 1 2]
t25 = adj g10 1 ~?= fromList [Edge 1 2,Edge 1 3,Edge 1 4]
{-| Testes unitários para a função transpose
-}
t26 = transpose g3 ~?= g7
t27 = transpose g0 ~?= g0
t28 = transpose g1 ~?= g1
{-| Testes unitários para a função union
-}
t29 = Graph.union g3 g6 ~?= g3
t30 = Graph.union g2 g8 ~?= g9
{-| Testes unitários para a função bft
-}
t31 =bft g9 (fromList[3]) ~?= Graph {nodes = fromList[3],edges = fromList []}
t32 = bft g10 (fromList[1]) ~?= Graph {nodes = fromList[1,2,3,4],edges = fromList[Edge 2 1,Edge 3 1,Edge 4 1]} 
t33 = bft g1 (fromList[]) ~?= Graph {nodes = fromList[],edges = fromList[]} 
{-| Testes unitários para a função reachable
-}
t34 = reachable g11 1 ~?= fromList[1,2,3,4,5]
t35 = reachable g10 2 ~?= fromList[2]
{-| Testes unitários para a função isPathOf
-}
t36 = isPathOf [Edge 1 2,Edge 1 3] g10 ~?= False
t37 = isPathOf [Edge 1 2,Edge 2 3,Edge 3 1] g3 ~?= True
t38 = isPathOf [Edge 1 4] g11 ~?= False
t39 = isPathOf [] g1 ~?= True
{-| Testes unitários para a função path
-}
t40 = path g11 1 4 ~?= Just [Edge 1 3,Edge 3 4]
t41 = path g11 3 5 ~?= Nothing
t42 = path g12 1 5 ~?= Just [Edge 1 2, Edge 2 5]
t43 = path g6 1 1 ~?= Just []
{-| Testes unitários para a função topo
-}
t44 = topo g10 ~?= [fromList [1],fromList [2,3,4]]
t45 = topo g11 ~?= [fromList [1],fromList[2,3],fromList[4,5]]
t46 = topo g12 ~?= [fromList [1],fromList[2,3],fromList[4],fromList[5]]
t47 = topo g0 ~?= []

alltests = TestList [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47]          
main = runTestTT alltests 
--
-- Teste aleatório
--

--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--

-- Instância de Arbitrary para arestas
instance Arbitrary v => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary
                   return $ Edge {source = s, target = t}

geraEdgesAleatorios :: [v] -> Gen (Edge v)
geraEdgesAleatorios edg = do s <- elements edg 
                             t <- elements edg
                             return (Edge s t)

instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
    arbitrary = do ns <- arbitrary
                   es <- listOf (geraEdgesAleatorios ns)
                   case ns of 
                    [] -> return $ Graph {nodes = fromList [], edges = fromList []}
                    _ -> return $ Graph {nodes = fromList ns, edges = fromList es}
                

prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = do n <- arbitrary 
         case n of 
          [] -> return $ Graph {nodes = Set.empty , edges = Set.empty}
          _ -> do num <- choose (1,100)
                  ed <- replicateM num (geraEdgesAleatorios n)
                  let g = Graph {nodes = fromList n , edges = fromList ed} 
                   in case (isDAG g) of 
                     True -> return g
                     False -> dag 

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = do n <- arbitrary 
            case n of 
             [] -> return $ Graph {nodes = Set.empty , edges = Set.empty}
             _ -> do num <- choose (1,100)
                     ed <- replicateM num (geraEdgesAleatorios n)
                     let g = Graph {nodes = fromList n , edges = fromList ed} 
                      in case (isForest g) of 
                        True -> return g
                        False -> forest 

prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g

prop_swap :: Edge Int -> Property
prop_swap e = property(swap(swap e) == e )

prop_empty :: Graph Int -> Bool
prop_empty g = g {nodes = Set.empty, edges = Set.empty} == g0


prop_isEmpty :: Graph Int -> Bool
prop_isEmpty g = if (isEmpty g)
                 then g == g0
                 else length(elems(nodes g)) /= 0

prop_isValid :: Graph Int -> Bool
prop_isValid g | ((Set.map source (edges g) `isSubsetOf` (nodes g)) && (Set.map target (edges g) `isSubsetOf` (nodes g))) == True = isValid g == True
               | otherwise = (isValid g == False)

verificaDAG :: [Edge Int] -> Graph Int -> Bool
verificaDAG [] g = True 
verificaDAG (h:t) g  = if( source h `elem`(elems(reachable g (target h))))
                       then False
                       else verificaDAG t g 

prop_isDAG :: Graph Int -> Bool
prop_isDAG g = isDAG g == verificaDAG (elems(edges g)) g

verificaForest :: [Edge Int]->[Edge Int]->Bool
verificaForest [] l= True
verificaForest (h:t) l = if (elem h l ) then False else verificaForest t (h:l)
                
prop_isForest :: Graph Int -> Property
prop_isForest g = forAll (dag :: Gen(DAG Int)) $ \v -> isForest v ==> forAll (elements $ elems $ nodes v) $ \d -> length (adj v d) <= 1

prop_isSubgraphOf :: Graph Int -> Graph Int -> Bool
prop_isSubgraphOf g1 g2 = (isSubgraphOf g1 g2 ) == ( all(\v -> v `elem`(elems(nodes g2))) (elems(nodes g1)) && all(\v -> v `elem`(elems(edges g2))) (elems(edges g1)) )

prop_tranpose :: Graph Int -> Bool
prop_tranpose e = transpose(transpose e) == e 

prop_union :: Graph Int -> Graph Int -> Graph Int ->  Bool
prop_union a b t = x == t {nodes = nodes a `Set.union` nodes b , edges = edges a `Set.union` edges b}
                 where x = Graph.union a b

prop_bft :: Graph Int -> Bool
prop_bft g = isForest (bft g (nodes g))

prop_reachable :: Graph Int -> Int -> Bool
prop_reachable g n = if (isEmpty g)  
                     then  True
                     else all (\x -> elems(nodes(bft g (singleton x)))/=[]) (elems(reachable g n))    

prop_isPathOf :: Graph.Path Int -> Graph Int -> Bool
prop_isPathOf l g = let v = isPathOf l g 
                    in  v == aux l (elems(edges g))
              where aux :: [Edge Int] -> [Edge Int] -> Bool
                    aux [] _ = True
                    aux (x:xs) l = case (x `elem` l) of 
                                       True -> aux xs l
                                       False -> False

prop_path :: Ord v => Graph v -> v -> v -> Property
prop_path g v1 v2 = property $ isSubsetOf (fromList(concat(maybeToList(Graph.path g v1 v2))))  (edges(transpose (bft g (fromList [v1]))))

prop_topo :: DAG Int -> Property
prop_topo d = forAll (dag) auxtopo 
     where auxtopo :: Graph Int -> Property
           auxtopo g = case isEmpty g of 
                         True -> property $ topo g == []
                         False -> case Set.null (edges g) of
                                    True -> property $ topo g == [nodes g]
                                    False -> forAll (elements $ elems $ nodes g) (\n -> prop_topo g)












