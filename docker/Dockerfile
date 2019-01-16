FROM erlang:21.2-alpine as builder
MAINTAINER tkyshm

ARG app_name=pnsvc

RUN apk add --no-cache --update git curl openssh

RUN set -xe \
    && git clone https://github.com/tkyshm/${app_name}.git \
    && cd ${app_name} \
    && rebar3 as prod release

FROM erlang:21.2-alpine

ARG version=0.1.0
ARG app_name=pnsvc

COPY --from=builder /${app_name}/_build/prod/rel /rel
COPY priv /rel/${app_name}/lib/${app_name}-${version}/priv

ENTRYPOINT ["/rel/pnsvc/bin/pnsvc"]
