FROM ghcr.io/mazharenko/dotnet-interactive-docker:1.0.358001

COPY --chown=1000 ./notebooks ${HOME}/Notebooks/

WORKDIR ${HOME}/Notebooks/