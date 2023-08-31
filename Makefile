IMAGE=ru-address-parser

.PHONY:
help:
	@echo ""
	@echo "Подсказка"
	@echo "---------"
	@echo ""
	@echo "Создание docker-образа:"
	@echo "    make docker-build"
	@echo ""
	@echo "Запуск HTTP-сервера в docker:"
	@echo "    make docker-run"
	@echo ""
	@echo "Компиляция проекта:"
	@echo "    make build"
	@echo ""
	@echo "Установка проекта в систему:"
	@echo "    make install"
	@echo ""
	@echo "Запуск приложения через cabal:"
	@echo "    make run"
	@echo ""
	@echo "Запуск HTTP-сервера через cabal:"
	@echo "    make run-server"
	@echo ""

.PHONY:
docker-build:
	docker build -t $(IMAGE) .

.PHONY:
docker-run:
	docker run --rm -it --name ru-address-parser -p 8000:8000 $(IMAGE)

.PHONY:
build:
	cabal build

.PHONY:
install:
	cabal install

.PHONY:
run:
	cabal run ru-address-parser --

.PHONY:
run-server:
	cabal run ru-address-parser-server -- -p 8000
