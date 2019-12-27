# entity-core

A Clojure library for defining domain types, their keys and persistence
bindings. In future, lifecycle management within transactions. Can be used
with [`entity-sql`](https://github.com/inqwell/entity-sql) when the persistence mechanism
is a SQL database. Uses [`typeops`](https://github.com/inqwell/typeops) to maintain purity of
domain type instances.

Documentation can be found [here](https://inqwell.github.io/entity-core/index.html)

See also [`entity-txn`](https://github.com/inqwell/entity-txn) for lifecycle management
and transactions

## Usage

`[entity/entity-core "0.1.2"]`

## License

Copyright © 2018 Inqwell Ltd

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
