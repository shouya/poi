IMAGE:=poi-linux-x64
EXE:=poi-exe
EXE_TARGET:=bin/poi

all: $(EXE_TARGET)

.PHONY: clean

.docker_built: Dockerfile
	docker build -t $(IMAGE) .
	touch .docker_built

$(EXE_TARGET): .docker_built $(wildcard src/*) *.cabal stack.yaml
	docker run -v `pwd`:/tmp \
	           -v `pwd`/.stack:/root/.stack \
	           -w /tmp \
	           $(IMAGE) \
	           stack build
	find .stack-work/dist/x86_64-linux \
	           -name "$(EXE)" -type f \
	           -exec cp -f {} $(EXE_TARGET) \;

clean:
	docker rmi -f $(IMAGE)
