IMAGE:=poi-linux-x64
EXE:=poi-exe
EXE_TARGET:=bin/poi
.PHONY: docker

all: $(EXE)

docker: Dockerfile
	docker build -t $(IMAGE) .

$(EXE): docker
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
