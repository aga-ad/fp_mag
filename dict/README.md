## Словарь, реализованный бором

```haskel
get :: String -> Dictionary -> Maybe String -- выдает значение по ключу
put :: String -> String -> Dictionary -> (Maybe String, Dictionary) -- заменяет или добавляет значение с заданным ключом
remove :: String -> Dictionary -> (Maybe String, Dictionary) -- удаляет слово из словаря
foldd :: ((String, String) -> a -> a) -> a -> Dictionary -> a -- свертка словаря в порядке убывания ключей
empty :: Dictionary -- создание пустого словаря
```
Запуск тестов
```
stack test
```
