# gwharness — local multi-comet regtest harness for Confidential Comets.
#
# All mutable state lives under run/ (gitignored). Commands shell into the
# `gwharness` python package.

PY := python3
H  := $(PY) -m gwharness

.PHONY: test selftest net-up net-down mine info comets scenario scenario-all dashboard tmux results clean

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

comets:          ## boot N=2 real comets (M1+)
	$(H) comets $(or $(N),2)

scenario:        ## run scenario S=first-contact (M3+)
	$(H) scenario $(S)

scenario-all:    ## run the full suite (M3+)
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
