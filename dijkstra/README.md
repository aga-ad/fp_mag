## Поиск кратчайшего расстояния между двумя вершинами графа с помощью алгоритма Дейкстры 

```haskell
type Arc = (String, Integer)
type Graph = String -> [Arc]
distance :: Graph -> String -> String -> Maybe Integer 
```

Исходный код src/Lib.hs
 
Тесты test/Spec.hs

# Запуск тестов
```
stack test
```
