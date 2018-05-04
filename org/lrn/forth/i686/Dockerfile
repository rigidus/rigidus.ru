FROM i686/ubuntu

RUN apt-get update \
&& apt-get install -y libsdl2-dev \
&& apt-get install -y make \
&& apt-get install -y g++
COPY SDL2 SDL2
COPY forth forth
COPY forth forth2
COPY fctf fctf

# build:  docker build -t rigidus/i686 .
# on host:   xhost +local:docker
# on host:   sudo docker run -it --env="DISPLAY" --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw" rigidus/i686 bash
# note: https://habrahabr.ru/post/240509/
# image from https://hub.docker.com/u/i686/
