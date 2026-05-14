FROM haskell:9.6.7-slim AS build

WORKDIR /opt/kbg

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./kbg.cabal /opt/kbg/kbg.cabal

RUN apt-get update && apt-get install postgresql-common -y && \
    /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh -y && \
    apt-get update && apt-get install libpq-dev -y

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/kbg
RUN cabal install --installdir=.

FROM debian:bookworm-slim

RUN apt-get update && apt-get install postgresql-common -y && \
    /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh -y && \
    apt-get update && apt-get install libpq-dev -y

COPY --from=build /opt/kbg/kbg /

CMD ["/kbg"]