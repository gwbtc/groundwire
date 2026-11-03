.PHONY: build clean gw-btc groundwire dist-groundwire-shim spv gw-onboard

ONBOARD_DIR := onboarding/booting
ONBOARD_VENV := $(ONBOARD_DIR)/.venv
ONBOARD_BIN := $(ONBOARD_DIR)/dist/gw-onboard

# Vendor files needed by the gw-btc desk
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
	mar/jam.hoon \
	mar/mime.hoon \
	mar/hoon.hoon \
	mar/txt.hoon \
	sur/asn1.hoon \
	sur/spider.hoon \
	sur/verb.hoon \
	mar/ship.hoon

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
	sur/spider.hoon \
	mar/ship.hoon

# Vendor files needed by spv-wallet desk (btc-wall subset)
VENDOR_BTC_WALL_SPV := \
	sur/resource.hoon \
	sur/btc-wallet.hoon

# gw-btc desk files needed by spv-wallet desk
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
		--hidden-import bitstring.bitstore_bitarray \
		--hidden-import bitstring.bitstore_bitarray_helpers \
		--hidden-import bitstring.bitstore_common_helpers \
		--hidden-import bitstring.bitstore_tibs \
		--hidden-import bitstring.bitstore_tibs_helpers \
		--hidden-import bitarray \
		--hidden-import bitarray._bitarray \
		--hidden-import bitarray._util \
		--hidden-import nacl._sodium \
		--hidden-import mmh3 \
		gw-onboard.py
	@echo "gw-onboard binary built at $(ONBOARD_BIN)"

gw-onboard: $(ONBOARD_BIN)

# A desk must contain NO file whose extension has no /mar/<ext>/hoon.  Clay
# needs a mark for every file it takes, and neither this desk nor
# vendor/base-dev has one for `md`.  The failure is SILENT: measured on a live
# ship, a desk with a single .md file in it committed to no new revision and
# printed nothing at all, while the same desk with the .md removed went 2 -> 3.
# So both the pill job's rsync into Clay's mount and deploy-desk.yml's
# `|commit %gw-btc` would no-op, leaving a desk with no %gw-btc agent in it and
# no error anywhere to say so.
#
# The documentation that used to trip this now lives in ops/doc/, outside the
# desk entirely, so the `rm -rf` below has nothing left to delete.  It stays as
# a net: the day someone adds README.md to the desk, this is what saves them.
# Anything else added under gw-btc/ needs a mark.
gw-btc:
	@rm -rf dist-gw-btc
	@mkdir -p dist-gw-btc
	@echo "Building gw-btc desk..."
	@cp -r gw-btc/* dist-gw-btc/
	@rm -rf dist-gw-btc/doc
	@for f in $(VENDOR_BASE_DEV_GW); do \
		mkdir -p dist-gw-btc/$$(dirname $$f); \
		cp vendor/base-dev/$$f dist-gw-btc/$$f; \
	done
	@for f in $(VENDOR_BTC_WALL_GW); do \
		mkdir -p dist-gw-btc/$$(dirname $$f); \
		cp vendor/btc-wall/$$f dist-gw-btc/$$f; \
	done
	@for f in $(VENDOR_TLON_LIB_GW); do \
		mkdir -p dist-gw-btc/$$(dirname $$f); \
		cp vendor/tlon-lib/$$f dist-gw-btc/$$f; \
	done
	@echo "gw-btc desk built."
	@$(MAKE) --no-print-directory dist-groundwire-shim

# TRANSITIONAL, DELETE AFTER gwbtc/urbit#67 MERGES.
#
# The release workflows on gwbtc/urbit's default branch run `make groundwire`
# and read `dist-groundwire/`.  They build the desk from THIS repo's `main`, so
# the moment the rename lands on main they would break -- and #129 merges to
# main BEFORE #67 carries the matching workflow change.  Rather than open that
# window and hope nobody runs a release inside it, we keep answering to the old
# names for one cycle.
#
# This is a REAL COPY, never a symlink.  daily-release.yml copies the desk with
# `rsync -a --no-links`, which SKIPS symlinks -- it would populate an empty
# desk and report success, which is exactly the class of failure this project
# keeps getting bitten by.  A wasted 2 MB is the correct price.
#
# Removal checklist: #67 merged, both workflows on the default branch say
# `gw-btc` / `dist-gw-btc`, one release cut green.  Then delete this target,
# the `groundwire` alias below, and their two lines from .gitignore.
dist-groundwire-shim:
	@rm -rf dist-groundwire
	@cp -r dist-gw-btc dist-groundwire
	@echo "  (compat: dist-groundwire/ mirrored for the pre-#67 workflows)"

groundwire: gw-btc

spv:
	@rm -rf dist-spv
	@mkdir -p dist-spv
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
		cp gw-btc/$$f dist-spv/$$f; \
	done
	@echo "spv-wallet desk built."

build: gw-btc spv gw-onboard
	@echo "Build completed successfully."

clean:
	rm -rf dist-gw-btc dist-groundwire dist-spv
	rm -rf $(ONBOARD_DIR)/dist $(ONBOARD_DIR)/build $(ONBOARD_DIR)/gw-onboard.spec
