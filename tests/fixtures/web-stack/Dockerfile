FROM ubuntu:22.04
RUN apt-get update && apt-get install -y curl
WORKDIR /srv
COPY . /srv
CMD ["bash", "-c", "echo hello"]
