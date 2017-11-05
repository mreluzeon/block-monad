# monabλock
Distributed system of cloud bees written in Cloud Haskell.

## Installation
```
git clone https://github.com/mreluzeon/block-monad
cd block-monad
stack build
```

## Execution
`stack exec blockmonad-server-exe [profile]`
`profile` is the name of the node config you want to load. Available profiles are: `John`, `Jacob` and `Sarah`. You can find them in the `configs` directory. The config follows this structure:
```
port of the node
ports of other nodes in the network separated by spaces
web interface port
```

## Usage
Once you have started your nodes, you can send them new flower coordinates via `stack exec client-exe <port>`, where `port` is the port of web interface of the node you wish to send coordinates.

