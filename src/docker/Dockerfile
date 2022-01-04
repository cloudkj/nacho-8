# Based on https://github.com/fcwu/docker-ubuntu-vnc-desktop
FROM dorowu/ubuntu-desktop-lxde-vnc:bionic

RUN apt-get update && \
    add-apt-repository ppa:allegro/5.2 && \
    apt-get install -y chicken-bin gcc make && \
    apt-get install -y libreadline-dev && \
    apt-get install -y liballegro*5.2 liballegro*5-dev && \
    chicken-install allegro format getopt-long input-parse readline

# Configure csi to use readline
# https://wiki.call-cc.org/eggref/4/readline#examples
RUN echo '(use readline)' > /root/.csirc
RUN echo '(current-input-port (make-readline-port))' >> /root/.csirc

CMD ["csi", "-version"]
