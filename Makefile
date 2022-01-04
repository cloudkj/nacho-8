PROGRAM_NAME=nacho-8

DOCKER=docker
DOCKER_IMAGE=cloudkj/nacho-8
DOCKER_RUN_FLAGS=-it -v "$$PWD:/home/nacho-8" -w /home/nacho-8
DOCKER_RUN_DESKTOP_FLAGS=-p 3000:80

CSC=csc

SRC_DIR=src
BUILD_DIR=build

build: $(SRC_DIR)/*.scm
	mkdir -p $(BUILD_DIR)
	$(DOCKER) run $(DOCKER_RUN_FLAGS) --entrypoint $(CSC) $(DOCKER_IMAGE) $(SRC_DIR)/*.scm -o $(BUILD_DIR)/$(PROGRAM_NAME)

shell:
	$(DOCKER) run $(DOCKER_RUN_FLAGS) --entrypoint bash $(DOCKER_IMAGE)

desktop:
	$(DOCKER) run $(DOCKER_RUN_FLAGS) $(DOCKER_RUN_DESKTOP_FLAGS) $(DOCKER_IMAGE)

clean:
	rm -rf $(BUILD_DIR)

docker-image:
	$(DOCKER) build -t $(DOCKER_IMAGE) src/docker
