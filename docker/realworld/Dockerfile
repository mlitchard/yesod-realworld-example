FROM fpco/stack-build

RUN    apt-get update \
    && apt-get install iputils-ping vim \
        && git clone https://github.com/mlitchard/yesod-realworld-example.git

WORKDIR /yesod-realworld-example

EXPOSE 3000

RUN stack setup

CMD  ["stack exec yesod-realworld-example"]