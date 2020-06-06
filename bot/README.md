# Бот для сбора данных

## Сборка и запуск

``` sh
cabal v2-update
cabal v2-build
cabal v2-run -- tgcec config.json
```

## Настройка доступа к гугл-таблице

- Открыть консоль разработчика Google https://console.developers.google.com/
- Включить Google Sheets API
- Добавить сервисный аккаунт https://console.developers.google.com/iam-admin/serviceaccounts
- Скачать файл с credentials и положить его куда-то в безопасное место
- При запуске указать путь к этому файлу в переменной `GOOGLE_APPLICATION_CREDENTIALS`
