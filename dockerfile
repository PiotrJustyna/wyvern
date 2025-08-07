FROM alpine:3.22.1

WORKDIR "/root/code/wyvern"

RUN apk update
RUN apk add \
    bash \
    git \
    openssh \
    curl \
    zlib-dev \
    g++ \
    ghc \
    cabal \
    ncurses-dev
RUN git config --global --add safe.directory "/root/code/wyvern"

RUN echo 'export PS1="\[\e[32m\]\w\[\e[35m\]\$\[\e[0m\] "' >> /root/.bashrc

# 2024-12-02 PJ:
# --------------
# this is where executables
# installed with cabal install are located
ENV PATH="${PATH}:/root/.local/bin"

COPY setup-cabal.sh /root/code/wyvern/

ENTRYPOINT ["/root/code/wyvern/setup-cabal.sh"]

CMD [ "/bin/bash" ]
