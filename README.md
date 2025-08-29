# Groundwire

Bitcoin-based Urbit networking.

## Getting Started

Run `./build.sh` to build the `/dist` folder with [peru](https://github.com/buildinspace/peru). (See [`urbit/desk-skeleton`](https://github.com/urbit/desk-skeleton) for more usage options.)

To run Groundwire on a fake ship, you'll need a pill with Arvo and Vere changes.

1. Boot a normal fake ship
2. `> |new-desk %gw-base`
3. `$ cp -rL path/to/pkg/arvo/* path/to/gw-base`
4. `> |commit %gw-base`
5. `> .gw-base/pill +pill/solid %gw-base`
6. Build [`gwbtc/vere`](https://github.com/gwbtc/vere)
  - Make sure you've cloned [`gwbtc/urcrypt`](https://github.com/gwbtc/urcrypt) in the same parent folder as `/vere` before building
7. `$ path/to/vere/zig-out/<platform>/urbit -F zod -B path/to/gw-base.pill`
8. `> |new-desk %groundwire`
9. `> |mount %groundwire`
10. `$ cp -rL path/to/groundwire/dist/* path/to/zod/groundwire`
11. `> |commit %groundwire`
12. `> |install our %groundwire`

