#!/usr/bin/env bash

# Parse arguments
COPY_PATH=""
COMMAND=""

while [[ $# -gt 0 ]]; do
  case $1 in
    -p)
      if [[ -z "$2" ]]; then
        echo "Error: -p flag requires a filepath argument" >&2
        exit 1
      fi
      COPY_PATH="$2"
      shift 2
      ;;
    *)
      if [[ -n "$COMMAND" ]]; then
        echo "Error: only one command is allowed" >&2
        exit 1
      fi
      COMMAND="$1"
      shift
      ;;
  esac
done

# Set default command if none provided
if [[ -z "$COMMAND" ]]; then
  COMMAND="build"
fi

check_peru_installed() {
  if ! command -v peru &> /dev/null; then
    echo "Error: peru is not installed or not in PATH." >&2
    echo "See: https://github.com/buildinspace/peru" >&2
    exit 1
  fi
}

copy_to_path() {
  local target_path="$1"
  
  if [[ ! -d dist ]]; then
    echo "Error: dist directory not found. Run build first." >&2
    exit 1
  fi
  
  if [[ ! -d "$target_path" ]]; then
    echo "Error: target path '$target_path' does not exist." >&2
    exit 1
  fi
  
  echo "Cleaning desk at $target_path..."
  rm -rf "$target_path"/*
  
  echo "Copying dist to $target_path..."
  cp -r dist/* "$target_path"/
  
  echo "Copy completed successfully."
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
  
  # Copy to specified path if -p flag was used
  if [[ -n "$COPY_PATH" ]]; then
    copy_to_path "$COPY_PATH"
  fi
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

case "$COMMAND" in
  build)
    build
    ;;
  build-dev)
    build_dev
    ;;
  help)
    echo "Usage: $0 [-p path] [build|build-dev|clean|help]"
    echo
    echo "  build       : build full desk from desk, desk-dev and dependencies in peru.yaml"
    echo "  build-dev   : build developer desk from desk-dev and dependencies in peru-dev.yaml"
    echo "  clean       : clean up dist and dist-dev"
    echo
    echo "Options:"
    echo "  -p path     : after building, copy dist contents to the desk at this path"
    echo "                (removes existing contents of the desk)"
    echo
    echo "  If no command is given, build is the default."
    echo "  Note: peru must be installed and available in PATH."
    echo "        See: https://github.com/buildinspace/peru"
    ;;
  clean)
    clean
    ;;
  *)
    echo "Error: unknown command '$COMMAND'" >&2
    exit 1
    ;;
esac

