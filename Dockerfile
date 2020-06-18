FROM gitpod/workspace-full

RUN bash -c ". /home/gitpod/.sdkman/bin/sdkman-init.sh \
             && sdk install java 11.0.7-open \
             && sdk install java 20.1.0.r11-grl"

RUN bash -c "brew update && brew install leiningen"
RUN bash -c "brew tap linuxbrew/xorg && brew install boot-clj"
