FROM fpco/stack-build-small:lts-16.31 AS cgidoc
RUN apt-get update && apt-get install -y libfcgi-dev && \
    rm -rf /var/lib/apt/lists/*
WORKDIR /tmp/cgidoc
COPY stack.yaml *.cabal ./
RUN stack build --system-ghc --dependencies-only
COPY *.hs ./
RUN stack install --system-ghc && rm -rf .stack-work

FROM ubuntu:18.04
RUN apt-get update && apt-get install -y spawn-fcgi libfcgi-bin libgmp10 && \
    rm -rf /var/lib/apt/lists/*
COPY --from=cgidoc /root/.local/bin/cgidoc /usr/local/bin/cgidoc
EXPOSE 7361
CMD ["spawn-fcgi","-n","-p","7361","-u","nobody","/usr/local/bin/cgidoc"]
