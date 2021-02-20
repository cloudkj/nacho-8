FROM ubuntu

RUN apt-get update && \
    apt-get install -y chicken-bin gcc libblas-dev bc ruby-full make && \
    apt-get install -y libreadline-dev libx11-dev && \
    chicken-install ezxdisp format getopt-long input-parse readline

# Configure csi to use readline
# https://wiki.call-cc.org/eggref/4/readline#examples
RUN echo '(use readline)' > /root/.csirc
RUN echo '(current-input-port (make-readline-port))' >> /root/.csirc

CMD ["csi", "-version"]
