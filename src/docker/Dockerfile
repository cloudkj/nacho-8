FROM ubuntu

RUN apt-get update && \
    apt-get install -y chicken-bin gcc libblas-dev bc ruby-full make && \
    apt-get install -y libx11-dev && \
    chicken-install ezxdisp format getopt-long input-parse

CMD ["csi", "-version"]
