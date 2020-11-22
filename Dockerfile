FROM ylecornec/alpine_with_ocsigen_start as builder

USER root
RUN apk add --no-cache python3
RUN apk add --no-cache g++
RUN ln -s /usr/bin/python3 /usr/bin/python
RUN mkdir -p /tmp/client/words_client/
RUN chown -R postgres:postgres /tmp/client/

USER postgres
WORKDIR /tmp/client/words_client/
COPY --chown=postgres:postgres client/words_client/package.json .
COPY --chown=postgres:postgres client/words_client/package-lock.json* .
RUN npm install

FROM ylecornec/alpine_with_ocsigen_start
COPY --from=builder /tmp/client/words_client/ /tmp/client/words_client/
COPY --chown=postgres:postgres words /tmp/words
COPY --chown=postgres:postgres client /tmp/client

WORKDIR /tmp/client/words_client/
RUN npm run build
RUN ./node_modules/.bin/webpack
RUN cp bundleOutput/index.js /tmp/words/static/js/index.js

WORKDIR /tmp/words
EXPOSE 8080
CMD ./launch.sh
