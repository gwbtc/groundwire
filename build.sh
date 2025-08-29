#!/usr/bin/env bash

# Fail if more than one argument
if [[ $# -gt 1 ]]; then
  echo "Error: only one argument is allowed" >&2
  exit 1
fi

check_peru_installed() {
  if ! command -v peru &> /dev/null; then
    echo "Error: peru is not installed or not in PATH." >&2
    echo "See: https://github.com/buildinspace/peru" >&2
    exit 1
  fi
}

build() {
  check_peru_installed

  if [[ ! -d desk && ! -d desk-dev ]]; then
    echo "Error: neither desk nor desk-dev directory found." >&2
    exit 1
  fi

  if [[ -d dist ]]; then
    echo "Removing existing dist directory..."
    rm -rf dist
  fi

  echo "Creating dist directory..."
  mkdir -p dist

  if [[ -d desk-dev ]]; then
    echo "Copying desk-dev → dist..."
    cp -r desk-dev/* dist/
  fi

  if [[ -d desk ]]; then
    echo "Copying desk → dist..."
    cp -r desk/* dist/
  fi

  echo "Running peru sync..."
  if ! peru sync 2>&1; then
    echo "Error: peru sync failed. Cleaning up dist..." >&2
    rm -rf dist
    exit 1
  fi

  echo "Build completed successfully."
}

build_dev() {
  check_peru_installed

  if [[ ! -d desk-dev ]]; then
    echo "Error: desk-dev directory not found." >&2
    exit 1
  fi

  if [[ -d dist-dev ]]; then
    echo "Removing existing dist-dev directory..."
    rm -rf dist-dev
  fi

  echo "Creating dist-dev directory..."
  mkdir -p dist-dev

  echo "Copying desk-dev → dist-dev..."
  cp -r desk-dev/* dist-dev/

  if [[ -f peru-dev.yaml ]]; then
    echo "Running peru sync..."
    if ! peru sync --file=peru-dev.yaml --sync-dir=./ 2>&1; then
      echo "Error: peru sync failed. Cleaning up dist-dev..." >&2
      rm -rf dist-dev
      exit 1
    fi
  fi

  echo "Dev build completed successfully."
}

clean() {
  if [[ -d dist ]]; then
    echo "Removing dist directory..."
    rm -rf dist
  fi
  if [[ -d dist-dev ]]; then
    echo "Removing dist-dev directory..."
    rm -rf dist-dev
  fi
}

case "$1" in
  "" | build)
    build
    ;;
  build-dev)
    build_dev
    ;;
  help)
    echo "Usage: $0 [build|build-dev|clean|help]"
    echo
    echo "  build       : build full desk from desk, desk-dev and dependencies in peru.yaml"
    echo "  build-dev   : build developer desk from desk-dev and dependencies in peru-dev.yaml"
    echo "  clean       : clean up dist and dist-dev"
    echo
    echo "  If no argument is given, build is the default."
    echo "  Note: peru must be installed and available in PATH."
    echo "        See: https://github.com/buildinspace/peru"
    ;;
  clean)
    clean
    ;;
  *)
    echo "Error: unknown argument '$1'" >&2
    exit 1
    ;;
esac

