.PHONY: all build run stop migrate clean release logs reset

# compile and build everything
all: compile release build

compile:
	rebar3 compile

release:
	rebar3 release

build:
	docker-compose build

run:
	docker-compose up

stop:
	docker-compose down

logs:
	docker-compose logs -f

# run the database migration
# This assumes that the database is already running
# and that the shared_storage app is already started
migrate:
	rebar3 shell --apps shared_storage --eval "db_migration:run()"

# clean up the build artifacts and volumes
clean:
	rebar3 clean
	docker-compose down --volumes

# clean and rebuild everything
reset:
	make clean
	make all
	make run