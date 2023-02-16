В пути к этой папке не используйте кириллицу. Если в комментариях видите крякозябры, откройте файл с кодировкой UTF-8.

Вам нужно реализовать функции, определённые в файлах в директории `src`. Порядок:
- FirstSteps.hs
- Lists.hs
- Luhn.hs
Для сборки лабораторной используется [`stack`](https://docs.haskellstack.org/en/stable/#quick-start-guide).
Он ставится вместе с [`ghcup`](https://www.haskell.org/ghcup/).
Для тестов в `test/Spec.hs` используется библиотека [`HSpec`](https://hspec.github.io/). Она будет автоматически скачана и установлена при использовании приведённых ниже команд.

Основные команды для этой лабораторной (остальные можно найти в документации):
- Собрать проект: `stack build`
- Открыть в REPL: `stack repl`
- Запустить все тесты: `stack test`
- Запустить один тест или группу по названию: `stack test --test-arguments "--match xor"`

Рекомендуемый редактор: [Visual Studio Code](https://code.visualstudio.com/) с расширением [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), но можно использовать и другие. Примерное описание установки можно найти [здесь (на английском)](https://betterprogramming.pub/haskell-vs-code-setup-in-2021-6267cc991551).
