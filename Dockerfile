FROM haskell:8.8.1 AS tll-build

WORKDIR /usr/src/tll-build
COPY tll.cabal .
COPY stack.yaml .
RUN stack build --only-snapshot

COPY web web
COPY src src
RUN stack build --copy-bins --local-bin-path /tmp/

FROM ubuntu:latest
WORKDIR /root/
COPY --from=tll-build /tmp/tll .
EXPOSE 4242
CMD ["./tll"] 
