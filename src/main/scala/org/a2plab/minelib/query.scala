package org.a2plab
package minelib

import java.io.{OutputStream, InputStream}
import java.net.{InetAddress, DatagramPacket, DatagramSocket}
import sbinary._
import sbinary.Input._
import sbinary.Output._
import sbinary.Operations._

trait RequestType {
  def description: String
  def byte: Byte
}

object RequestType {
  case object Handshake extends RequestType {
    val description = "basic"
    val byte = 0x09.toByte
  }
  case object Status extends RequestType {
    val description = "detailed"
    val byte = 0x00.toByte
  }

  def fromByte(byte: Byte) = byte match {
    case Handshake.byte => Handshake
    case Status.byte => Status
  }
}

case class HandshakeRequest(sessionId: Int)
case class HandshakeResponse(sessionId: Int, token: Int)

sealed trait StatsRequestType
sealed trait BasicStatsRequestType extends StatsRequestType
sealed trait FullStatsRequestType extends StatsRequestType

case class StatsRequest[T <: StatsRequestType](sessionId: Int, token: Int)
object StatsRequest {
  def basic(sessionId: Int, token: Int) = new StatsRequest[BasicStatsRequestType](sessionId, token)
  def full(sessionId: Int, token: Int) = new StatsRequest[FullStatsRequestType](sessionId, token)
}

case class BasicStatsResponse(
            sessionId: Int,
            motd: String,
            gameType: String,
            worldName: String,
            players: Int,
            supportedPlayers: Int,
            port: Short,
            host: String)

case class FullStatsResponse(
            sessionId: Int,
            properties: Map[String, String],
            players: List[String])

object QueryProtocol {
  import sbinary.DefaultProtocol.IntFormat
  import sbinary.DefaultProtocol.ShortFormat
  
  trait NullTerminatedStringReader {
    def readString(in: Input, last: List[Char] = Nil): String = {
      val next = in.readByte
      if (next == 0x00.toByte) last.reverse.mkString else readString(in, next.toChar :: last)
    }
  }

  implicit object ReadNullTerminatedString extends Reads[String] with NullTerminatedStringReader {
    def reads(in: Input) = readString(in)
  }
  
  implicit object ReadKeyValuePairOption extends Reads[Option[(String, String)]] with NullTerminatedStringReader {
    def reads(in: Input) = {
      val firstByte = in.readByte
      val secondByte = in.readByte
      
      val key = if (firstByte == 0x00.toByte && secondByte == 0x01.toByte) None else Some(readString(in, List(secondByte.toChar, firstByte.toChar)))
      
      key.map(k => (k, read[String](in)))
    }
  }
  
  implicit object ReadStringsMap extends Reads[Map[String, String]] {
    def readMap(in: Input, lastMap: Map[String, String] = Map.empty): Map[String, String] = read[Option[(String, String)]](in) match {
      case None => lastMap
      case Some(pair) => readMap(in, lastMap + pair) 
    }
    
    def reads(in: Input) = readMap(in)
  }
  
  implicit object ReadStringsList extends Reads[List[String]] {
    def readList(in: Input, lastList: List[String] = Nil): List[String] = {
      val nextString = read[String](in)
      if (nextString.isEmpty) lastList
      else readList(in, nextString :: lastList)
    }
    
    def reads(in: Input) = readList(in)
  }

  implicit object ReadHandshakeResponse extends Reads[HandshakeResponse] {
    def reads(in: Input) = {
      assert(RequestType.fromByte(in.readByte) == RequestType.Handshake, "Request type is not handshake")

      HandshakeResponse(
        read[Int](in),
        read[String](in).toInt
      )
    }
  }

  implicit object WriteHandshakeRequest extends Writes[HandshakeRequest] {
    def writes(out : Output, value : HandshakeRequest) {
      out.writeAll(Array(0xFE.toByte, 0xFD.toByte, RequestType.Handshake.byte))
      write[Int](out, value.sessionId)
    }
  }

  trait WriteStatsRequestCommon {
    def writeCommons(out: Output, value: StatsRequest[_]) {
      out.writeAll(Array(0xFE.toByte, 0xFD.toByte, RequestType.Status.byte))
      write(out, value.sessionId)
      write(out, value.token)
    }
  }

  implicit object WriteBasicStatsRequest extends Writes[StatsRequest[BasicStatsRequestType]] with WriteStatsRequestCommon {
    def writes(out : Output, value : StatsRequest[BasicStatsRequestType]) {
      writeCommons(out, value)
    }
  }

  implicit object WriteFullStatsRequest extends Writes[StatsRequest[FullStatsRequestType]] with WriteStatsRequestCommon {
    def writes(out: Output, value: StatsRequest[FullStatsRequestType]) {
      writeCommons(out, value)
      write(out, 0) // Be sure we are using four 0 bytes for padding
    }
  }

  implicit object ReadBasicStatsResponse extends Reads[BasicStatsResponse] {
    def reads(in: Input) = {
      assert(RequestType.fromByte(in.readByte) == RequestType.Status, "Request type is not status")

      BasicStatsResponse(
        read[Int](in),            // Session ID
        read[String](in),         // MoTD
        read[String](in),         // Game type
        read[String](in),         // Map
        read[String](in).toInt,   // Num players
        read[String](in).toInt,   // Max players
        read[Short](in),          // Host port
        read[String](in)          // Host address
      )
    }
  }
  
  implicit object ReadFullStatsResponse extends Reads[FullStatsResponse] {
    def reads(in: Input) = {
      assert(RequestType.fromByte(in.readByte) == RequestType.Status, "Request type is not status")
      
      val sessionId = read[Int](in)
      
      val splitnumString = read[String](in) 
      assert(splitnumString == "splitnum", "Expected hardcoded string 'splitnum', got '" + splitnumString + "'")
      
      val keyValStartFirstByte = in.readByte
      assert(keyValStartFirstByte == 0x80.toByte, "Expected byte 0x80, got " + "%02X".format(keyValStartFirstByte))
      
      val keyValStartSecondByte = in.readByte
      assert(keyValStartSecondByte == 0x00.toByte, "Expected byte 0x00, got " + "%02X".format(keyValStartSecondByte))
      
      val serverProperties = read[Map[String, String]](in)
      
      val playerString = read[String](in)
      assert(playerString == "player_", "Expected hardcoded string 'player_', got '" + playerString + "'")
      
      val paddingByte = in.readByte
      assert(paddingByte == 0x00.toByte, "Expected byte 0x00, got " + "%02X".format(paddingByte))
      
      val players = read[List[String]](in)
      
      FullStatsResponse(sessionId, serverProperties, players)
    }
  }
}

class QueryClient(val host: String, val port: Int) {
  import QueryProtocol._

  def withConnection[T](c: DatagramSocket => T) = {
    val sock = new DatagramSocket()
    sock.setSoTimeout(2000)
    val retrieved = c(sock)
    sock.close()

    retrieved
  }

  def datagramPacket(data: Array[Byte]) = new DatagramPacket(data, data.length, InetAddress.getByName(host), port)

  def handshake(sock: DatagramSocket): HandshakeResponse = {
    sock.send(datagramPacket(toByteArray(HandshakeRequest(1))))

    var receivedData = new Array[Byte](1024)
    val receivedPacket = new DatagramPacket(receivedData, receivedData.length);
    sock.receive(receivedPacket);

    fromByteArray[HandshakeResponse](receivedPacket.getData)
  }

  def basicStatus: BasicStatsResponse = withConnection { sock =>
    val handshakeResponse = handshake(sock)
    sock.send(datagramPacket(toByteArray(StatsRequest.basic(handshakeResponse.sessionId, handshakeResponse.token))))

    var receivedData = new Array[Byte](1024)
    val receivedPacket = new DatagramPacket(receivedData, receivedData.length)
    sock.receive(receivedPacket)

    fromByteArray[BasicStatsResponse](receivedPacket.getData)
  }
  
  def fullStatus: FullStatsResponse = withConnection { sock =>
    val handshakeResponse = handshake(sock)
    sock.send(datagramPacket(toByteArray(StatsRequest.full(handshakeResponse.sessionId, handshakeResponse.token))))

    var receivedData = new Array[Byte](4096)
    val receivedPacket = new DatagramPacket(receivedData, receivedData.length)
    sock.receive(receivedPacket)

    fromByteArray[FullStatsResponse](receivedPacket.getData)
  }
}
