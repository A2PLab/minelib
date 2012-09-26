# Minelib

Minelib is a simple scala library that allows to connect to the the Query and the RCon services on a minecraft server. The only
dependency is [sbinary](https://github.com/harrah/sbinary "sbinary on github") version 0.4.0 (see the build file for more details).

## Example of a QueryClient invocation:

    val client = new org.a2plab.minelib.QueryClient("my.server.ip", 25565) // Address, Port
    client.basicStatus // returns basic status information - see case class BasicStatsResponse
    client.fullStatus // return full status information - see case class FullStatsResponse

For more information about the Query protocol see [this wiki](http://wiki.vg/Query "wiki page about Query protocol")

## Example of a RConClient invocation:

    val client = new org.a2plab.minelib.RConClient("my.server.ip", 25575) // Address, Port
    client.command("my-server-rcon-password", "my-command-text") // returns an option containing the server's response for the given command

For more information about RCon protocol see: [this wiki](http://wiki.vg/Rcon "wiki page about RCon protocol")
