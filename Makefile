.PHONY: elvis test start-redis stop-redis dialyzer 

TAG ?= $(shell git log --format=%h -n 1)
APP_NAME := pnsvc

elvis:
	elvis --config elvis.config

dialyzer:
	rebar3 dialyzer

test: gen-service-account elvis dialyzer
	rebar3 ct -v

docker-release-build:
	cd docker/ && docker build -t $(APP_NAME):$(TAG) .

gen-service-account:
	./gen_service_account.sh
