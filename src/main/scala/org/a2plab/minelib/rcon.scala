package org.a2plab
package minelib

import java.net.{InetSocketAddress, Socket}
import sbinary._
import sbinary.Operations._
import java.io.{OutputStream, InputStream}

sealed trait RConFrameType {
  def description: String
  def typeCode: Int
}
object RConFrameType {
  case object Login extends RConFrameType {
    val description = "login"
    val typeCode: Int = 3
  }
  case object Command extends RConFrameType {
    val description = "command"
    val typeCode: Int = 2
  }

  def fromInt(typeNumber: Int) = typeNumber match {
    case Login.typeCode => Login
    case Command.typeCode => Command
  }
}

case class LoginRequest(requestId: Int, password: String)
case class LoginResponse(requestId: Int) { def isAuthSuccess = requestId != -1 }
case class CommandRequest(requestId: Int, command: String)
case class CommandResponse(requestId: Int, responseText: String)

object RConProtocol {
  import sbinary.DefaultProtocol.ByteFormat

  implicit object LittleEndianIntFormat extends Format[Int] {
    def writes(out: Output, value: Int) {
      out.writeAll(toByteArray(value)(sbinary.DefaultProtocol.IntFormat).reverse)
    }

    def reads(in: Input): Int = {
      val readBytes = new Array[Byte](4)
      in.readFully(readBytes)
      fromByteArray[Int](readBytes.reverse)(sbinary.DefaultProtocol.IntFormat)
    }
  }

  implicit object WritesLoginRequest extends Writes[LoginRequest] {
    def writes(out : Output, value: LoginRequest) {
      val length: Int = 10 + value.password.length
      write(out, length)
      write(out, value.requestId)
      write(out, RConFrameType.Login.typeCode)
      value.password.toList.map { ch => write(out, ch.toByte) }
      write(out, 0x00.toByte)
      write(out, 0x00.toByte)
    }
  }

  implicit object ReadsLoginResponse extends Reads[LoginResponse] {
    def reads(in: Input) = {
      val length = read[Int](in)
      val requestId = read[Int](in)

      val responseType = RConFrameType.fromInt(read[Int](in))
      assert(responseType == RConFrameType.Command, "Expected response type " + RConFrameType.Command.description + ", got " + responseType.description)

      val payloadArray = new Array[Byte](length - 10)
      in.readFully(payloadArray)

      val firstPaddingByte = in.readByte
      assert(firstPaddingByte == 0x00.toByte, "Expected 0x00 padding byte, got " + firstPaddingByte)

      val secondPaddingByte = in.readByte
      assert(secondPaddingByte == 0x00.toByte, "Expected 0x00 padding byte, got " + secondPaddingByte)

      LoginResponse(requestId)
    }
  }

  implicit object WritesCommandRequest extends Writes[CommandRequest] {
    def writes(out : Output, value : CommandRequest) {
      val length = 10 + value.command.length
      write(out, length)
      write(out, value.requestId)
      write(out, RConFrameType.Command.typeCode)
      value.command.toList.map { ch => write(out, ch.toByte) }
      write(out, 0x00.toByte)
      write(out, 0x00.toByte)
    }
  }

  implicit object ReadsCommandResponse extends Reads[CommandResponse] {
    def reads(in: Input) = {
      val length = read[Int](in)
      val requestId = read[Int](in)

      read[Int](in) // TODO: Understand why this value is not the expected one
//      val responseType = RConFrameType.fromInt(read[Int](in))
//      assert(responseType == RConFrameType.Command, "Expected response type " + RConFrameType.Command.description + ", got " + responseType.description)

      val payloadArray = new Array[Byte](length - 10)
      in.readFully(payloadArray)

      val firstPaddingByte = in.readByte
      assert(firstPaddingByte == 0x00.toByte, "Expected 0x00 padding byte, got " + firstPaddingByte)

      val secondPaddingByte = in.readByte
      assert(secondPaddingByte == 0x00.toByte, "Expected 0x00 padding byte, got " + secondPaddingByte)
println("PAL" + payloadArray.length)
      CommandResponse(requestId, payloadArray.map(_.toChar).mkString)
    }
  }
}

class RConClient(val host: String, val port: Int) {
  import RConProtocol._

  def withConnection[T](c: (InputStream, OutputStream) => T): T = {
    val sock = new Socket(host, port)
    sock.setSoTimeout(2000)
    val retrieved = c(sock.getInputStream, sock.getOutputStream)
    sock.close()

    retrieved
  }

  def login(is: InputStream, os: OutputStream, password: String): Option[Int] = {
    val b = toByteArray(LoginRequest(1, password))
    os.write(b)

    val response = read[LoginResponse](is)

    if (response.isAuthSuccess) Some(response.requestId) else None
  }

  def command(password: String, command: String): Option[String] = withConnection { (is, os) =>
    login(is, os, password) match {
      case Some(sessionId) =>
        os.write(toByteArray(CommandRequest(sessionId, command)))
        Some(read[CommandResponse](is).responseText)
      case None => None
    }
  }
}
