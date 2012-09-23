package org.a2plab
package minelib

import java.net.Socket
import sbinary._
import sbinary.Operations._

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
  import sbinary.DefaultProtocol.IntFormat
  import sbinary.DefaultProtocol.ByteFormat

  implicit object WritesLoginRequest extends Writes[LoginRequest] {
    def writes(out : Output, value : LoginRequest) {
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

      val payloadArray = new Array[Byte](length - 8)
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

      val responseType = RConFrameType.fromInt(read[Int](in))
      assert(responseType == RConFrameType.Command, "Expected response type " + RConFrameType.Command.description + ", got " + responseType.description)

      val payloadArray = new Array[Byte](length - 8)
      in.readFully(payloadArray)

      val firstPaddingByte = in.readByte
      assert(firstPaddingByte == 0x00.toByte, "Expected 0x00 padding byte, got " + firstPaddingByte)

      val secondPaddingByte = in.readByte
      assert(secondPaddingByte == 0x00.toByte, "Expected 0x00 padding byte, got " + secondPaddingByte)

      CommandResponse(requestId, payloadArray.map(_.toChar).mkString)
    }
  }
}

class RConClient(val host: String, val port: Int) {
  import RConProtocol._

  def withConnection[T](c: Socket => T): T = {
    val sock = new Socket(host, port)
    sock.setSoTimeout(2000)
    val retrieved = c(sock)
    sock.close()

    retrieved
  }

  def login(socket: Socket, password: String): Option[Int] = {
    val os = socket.getOutputStream

    val b = toByteArray(LoginRequest(1, password))
    b.map("%02X " format _).foreach(print)
    println("\nL: " + b.length + "\n")

    write(os, LoginRequest(1, password))
    os.flush()

    val response = read[LoginResponse](socket.getInputStream)

    if (response.isAuthSuccess) Some(response.requestId) else None
  }

  def command(password: String, command: String): Option[String] = withConnection { socket =>
    login(socket, password) match {
      case Some(sessionId) =>
        write(socket.getOutputStream, CommandRequest(sessionId, command))
        Some(read[CommandResponse](socket.getInputStream).responseText)
      case None => None
    }
  }
}

