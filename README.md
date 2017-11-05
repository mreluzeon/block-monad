# monabλock
Distributed system of cloud bees written in Cloud Haskell.
In this model the cluster is the hive, and the nodes are the bees. Once a scout bee has discovered an appetizing flower, it flies back to the hive and shares the coordinates of the flower with the other bees for them to be able to eat.

## Installation
```
git clone https://github.com/mreluzeon/block-monad
cd block-monad
stack build
```

## Starting the nodes
```
stack exec blockmonad-server-exe [profile]
```
`profile` is the name of the node config you want to load. Available profiles are: `John`, `Jacob` and `Sarah`. You can find them in the `configs` directory. The config follows this structure:
```
port of the node
ports of other nodes in the network separated by spaces
web interface port
```

## Usage (for scout bees)
Once you have started your nodes, you can send them new flower coordinates via `stack exec client-exe <port>`, where `port` is the port of web interface of the node you wish to send the coordinates.

## Implementation (for curious)
The hive database of appetizing flowers is a growing-only set, which is a CRDT data structure. Flowers are shared between nodes, and CRDT gives us a way to easily implement consensus between bees.
