IMAGE:=poi-linux-x64
EXE:=poi-exe
TAR:=tar
.PHONY: docker

all: $(EXE)

docker:
	docker build -t $(IMAGE) .

$(EXE): docker
	docker run -a stdout $(IMAGE) tar cf - /tmp/.stack-work/dist | \
		$(TAR) xf - --strip-components=4 -C bin

clean:
	docker rmi -f $(IMAGE)
