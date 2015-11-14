FROM gliderlabs/alpine:3.2

MAINTAINER shouyatf@gmail.com

RUN apk --update add alpine-sdk
RUN apk add zlib-dev openssl-dev iptables-dev

RUN mkdir /shadowsocks

RUN cd / && git clone https://github.com/madeye/shadowsocks-libev.git

WORKDIR /shadowsocks-libev

RUN ./configure
RUN make
RUN make install

ADD shadowsocks.sh  /
RUN chmod +x /shadowsocks.sh

EXPOSE 1

CMD /shadowsocks.sh
