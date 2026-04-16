.PHONY: build clean gw-onboard

ONBOARD_DIR := onboarding/booting
ONBOARD_VENV := $(ONBOARD_DIR)/.venv
ONBOARD_BIN := $(ONBOARD_DIR)/dist/gw-onboard

# Vendor files needed by groundwire desk
VENDOR_BASE_DEV_GW := \
	lib/bip32.hoon \
	lib/bip39.hoon \
	lib/bip39/english.hoon \
	lib/dbug.hoon \
	lib/der.hoon \
	lib/default-agent.hoon \
	lib/mip.hoon \
	lib/skeleton.hoon \
	lib/strand.hoon \
	lib/strandio.hoon \
	lib/test.hoon \
	lib/verb.hoon \
	lib/server.hoon \
	mar/bill.hoon \
	mar/kelvin.hoon \
	mar/noun.hoon \
	mar/mime.hoon \
	mar/hoon.hoon \
	sur/asn1.hoon \
	sur/spider.hoon \
	sur/verb.hoon

VENDOR_BTC_WALL_GW := \
	lib/group.hoon \
	lib/group-store.hoon \
	lib/resource.hoon \
	sur/group.hoon \
	sur/group-store.hoon \
	sur/resource.hoon \
	sur/btc-wallet.hoon

VENDOR_TLON_LIB_GW := \
	lib/test-agent.hoon

# Vendor files needed by spv-wallet desk (base-dev subset)
VENDOR_BASE_DEV_SPV := \
	lib/bip39.hoon \
	lib/bip39/english.hoon \
	lib/dbug.hoon \
	lib/der.hoon \
	lib/default-agent.hoon \
	lib/skeleton.hoon \
	lib/strand.hoon \
	lib/strandio.hoon \
	lib/test.hoon \
	mar/bill.hoon \
	mar/noun.hoon \
	mar/mime.hoon \
	mar/txt.hoon \
	mar/kelvin.hoon \
	mar/hoon.hoon \
	sur/asn1.hoon \
	sur/spider.hoon

# Vendor files needed by spv-wallet desk (btc-wall subset)
VENDOR_BTC_WALL_SPV := \
	sur/resource.hoon \
	sur/btc-wallet.hoon

# Groundwire desk files needed by spv-wallet desk
GW_FILES_FOR_SPV := \
	lib/bip/b158.hoon \
	lib/bip/b173.hoon \
	lib/bip/b174.hoon \
	lib/bip69.hoon \
	lib/bitcoin-utils.hoon \
	lib/bitcoin.hoon \
	lib/btc-script.hoon \
	lib/btcio.hoon \
	lib/groundwire.hoon \
	lib/ord.hoon \
	lib/psbt.hoon \
	lib/urb-encoder.hoon \
	sur/bitcoin.hoon \
	sur/btc-provider.hoon \
	sur/json/rpc.hoon \
	sur/ord.hoon \
	sur/psbt.hoon \
	sur/urb.hoon \
	tests/lib/bip32.hoon \
	tests/lib/bip39.hoon

$(ONBOARD_VENV):
	python3 -m venv $(ONBOARD_VENV)
	$(ONBOARD_VENV)/bin/pip install -q -r $(ONBOARD_DIR)/requirements.txt pyinstaller

$(ONBOARD_BIN): $(ONBOARD_VENV) $(ONBOARD_DIR)/gw-onboard.py
	@echo "Building gw-onboard binary..."
	cd $(ONBOARD_DIR) && .venv/bin/pyinstaller --onefile \
		--hidden-import requests \
		--hidden-import nacl.bindings \
		--hidden-import embit.util.secp256k1 \
		--hidden-import _cffi_backend \
		gw-onboard.py
	@echo "gw-onboard binary built at $(ONBOARD_BIN)"

gw-onboard: $(ONBOARD_BIN)

build:
	@rm -rf dist-groundwire dist-spv
	@mkdir -p dist-groundwire dist-spv
	@echo "Building groundwire desk..."
	@cp -r groundwire/* dist-groundwire/
	@for f in $(VENDOR_BASE_DEV_GW); do \
		mkdir -p dist-groundwire/$$(dirname $$f); \
		cp vendor/base-dev/$$f dist-groundwire/$$f; \
	done
	@for f in $(VENDOR_BTC_WALL_GW); do \
		mkdir -p dist-groundwire/$$(dirname $$f); \
		cp vendor/btc-wall/$$f dist-groundwire/$$f; \
	done
	@for f in $(VENDOR_TLON_LIB_GW); do \
		mkdir -p dist-groundwire/$$(dirname $$f); \
		cp vendor/tlon-lib/$$f dist-groundwire/$$f; \
	done
	@echo "Building spv-wallet desk..."
	@cp -r spv-wallet/* dist-spv/
	@for f in $(VENDOR_BASE_DEV_SPV); do \
		mkdir -p dist-spv/$$(dirname $$f); \
		cp vendor/base-dev/$$f dist-spv/$$f; \
	done
	@for f in $(VENDOR_BTC_WALL_SPV); do \
		mkdir -p dist-spv/$$(dirname $$f); \
		cp vendor/btc-wall/$$f dist-spv/$$f; \
	done
	@for f in $(GW_FILES_FOR_SPV); do \
		mkdir -p dist-spv/$$(dirname $$f); \
		cp groundwire/$$f dist-spv/$$f; \
	done
	@echo "Build completed successfully."
	@$(MAKE) $(ONBOARD_BIN)

clean:
	rm -rf dist-groundwire dist-spv
	rm -rf $(ONBOARD_DIR)/dist $(ONBOARD_DIR)/build $(ONBOARD_DIR)/gw-onboard.spec
