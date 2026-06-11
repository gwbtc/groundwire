# gwharness — local multi-comet regtest harness for Confidential Comets.
#
# All mutable state lives under run/ (gitignored). Commands shell into the
# `gwharness` python package.

PY := python3
H  := $(PY) -m gwharness

.PHONY: test selftest net-up net-down mine info m1 m2 scenario scenario-all dashboard tmux results clean

test:            ## full pytest suite (noun vs vere, bitcoind end-to-end)
	$(PY) -m pytest gwharness/tests -v

selftest:        ## fast offline plumbing checks (no ship/bitcoind)
	$(PY) -m pytest gwharness/tests -q -k "not live"

net-up:          ## start regtest bitcoind + funded wallet
	$(H) net-up

net-down:        ## stop bitcoind
	$(H) net-down

mine:            ## mine N=1 regtest blocks
	$(H) mine $(or $(N),1)

info:            ## chain tip + running piers
	$(H) info

m1:              ## single comet verifies its own keyfile end-to-end (VALID)
	$(H) m1

m2:              ## two comets: cross-verify packets, inject lanes, |hi both ways
	$(H) m2

scenario:        ## run only S=name1,name2 from the suite
	$(H) scenarios --only $(S)

scenario-all:    ## run the full adversarial suite (8 scenarios)
	$(H) scenario-all

dashboard:       ## serve the live dashboard on :42999 (M4)
	$(H) dashboard

tmux:            ## attach a tmux session over the running net (M4)
	bin/net-tmux.sh

results:         ## re-render run/RESULTS.md
	$(H) results

clean:           ## kill piers, stop bitcoind, remove run/
	-$(H) net-down
	-@for lock in run/piers/*/.vere.lock; do \
	   [ -f "$$lock" ] && kill `cat "$$lock"` 2>/dev/null || true; done
	rm -rf run
