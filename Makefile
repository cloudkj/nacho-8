PROGRAM_NAME=nacho-8

DOCKER=docker
DOCKER_IMAGE=cloudkj/nacho-8
DOCKER_RUN_FLAGS=-it -e DISPLAY=host.docker.internal:0 -v "$$PWD:/home/nacho-8" -w /home/nacho-8

CSC=csc

SRC_DIR=src
BUILD_DIR=build

build:
	mkdir -p $(BUILD_DIR)
	$(DOCKER) run $(DOCKER_RUN_FLAGS) $(DOCKER_IMAGE) $(CSC) $(SRC_DIR)/*.scm -o $(BUILD_DIR)/$(PROGRAM_NAME)

shell:
	$(DOCKER) run $(DOCKER_RUN_FLAGS) $(DOCKER_IMAGE) bash

clean:
	rm -rf $(BUILD_DIR)

docker-image:
	$(DOCKER) build -t $(DOCKER_IMAGE) src/docker
