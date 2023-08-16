FROM fpco/stack-build-small:lts-19.33 AS build
WORKDIR /tmp/cgidoc
COPY stack.yaml *.cabal ./
RUN stack build --system-ghc --dependencies-only
COPY *.hs ./
RUN stack install --system-ghc && rm -rf .stack-work

FROM ubuntu:18.04
COPY --from=build /root/.local/bin/cgidoc /usr/local/bin/cgidoc
VOLUME /run/cgidoc
CMD ["/usr/local/bin/cgidoc","/run/cgidoc/sock"]
