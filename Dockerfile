FROM ghcr.io/mazharenko/dotnet-interactive-docker:1.0.355307

COPY --chown=1000 ./notebooks ${HOME}/Notebooks/

WORKDIR ${HOME}/Notebooks/