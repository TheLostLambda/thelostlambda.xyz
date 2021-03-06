FROM haskell:9.0.2 AS tll-build

WORKDIR /usr/src/tll-build
COPY tll.cabal .
COPY stack.yaml .
RUN stack build --only-snapshot

COPY src src
RUN stack build --copy-bins --local-bin-path /tmp/

FROM debian:latest
RUN apt-get update && apt-get upgrade && apt-get install libnuma1
WORKDIR /root/
COPY web web
COPY --from=tll-build /tmp/tll .
EXPOSE 4242
VOLUME /var/log/
CMD ./tll 2>&1 | tee -a /var/log/tll.log
