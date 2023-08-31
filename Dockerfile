FROM haskell:9.2

WORKDIR /opt/app

RUN cabal update

COPY ./*.cabal /opt/app/

RUN cabal build --only-dependencies -j4

COPY . /opt/app
RUN cabal install

ENV PATH=$PATH:/root/.cabal/bin
CMD ru-address-parser-server -p 8000
