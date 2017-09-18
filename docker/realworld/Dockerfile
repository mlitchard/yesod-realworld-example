FROM fpco/stack-build

RUN    apt-get update \
    && apt-get install -y iputils-ping vim \
        && git clone https://github.com/mlitchard/yesod-realworld-example.git

WORKDIR /yesod-realworld-example

EXPOSE 3000

RUN stack setup && stack install

CMD  ["/root/.local/bin/yesod-realworld-example"]
