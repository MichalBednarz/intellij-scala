package org.jetbrains.jps.incremental.scala
package remote

import java.io._
import data.{CompilationData, CompilerData, SbtData}
import java.net.{UnknownHostException, ConnectException, Socket}
import com.martiansoftware.nailgun.NGConstants
import org.jetbrains.jps.incremental.messages.BuildMessage.Kind
import org.jetbrains.jps.incremental.ModuleLevelBuilder.ExitCode
import RemoteServer._

/**
 * @author Pavel Fatin
 */
class RemoteServer(address: String, port: Int) extends Server {
  def compile(sbtData: SbtData, compilerData: CompilerData, compilationData: CompilationData, client: Client): ExitCode = {
    val arguments = Arguments(sbtData, compilerData, compilationData).asStrings

    try {
      send(MainClass, arguments, client)
      ExitCode.OK
    } catch {
      case e: ConnectException =>
        val message = "Cannot connect to Naigun server at %s:%s".format(address, port)
        client.error(message)
        ExitCode.ABORT
      case e: UnknownHostException =>
        val message = "Unknown IP address of Nailgun server host: " + address
        client.error(message)
        ExitCode.ABORT
    }
  }

  private def send(command: String, arguments: Seq[String], client: Client) {
    using(new Socket(address, port)) { socket =>
      using(new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))) { output =>
        createChunks(command, arguments).foreach(_.writeTo(output))
        output.flush()
        using(new DataInputStream(new BufferedInputStream(socket.getInputStream))) { input =>
          handle(input, client)
        }
      }
    }
  }
}

private object RemoteServer {
  private val MainClass = "org.jetbrains.jps.incremental.scala.remote.Main"

  private val CurrentDirectory = System.getProperty("user.dir")

  private def createChunks(command: String, args: Seq[String]): Seq[Chunk] = {
    args.map(Chunk(NGConstants.CHUNKTYPE_ARGUMENT, _)) :+
            Chunk(NGConstants.CHUNKTYPE_WORKINGDIRECTORY, CurrentDirectory) :+
            Chunk(NGConstants.CHUNKTYPE_COMMAND, command)
  }

  private def handle(input: DataInputStream, client: Client) {
    val processor = new ClientEventProcessor(client)

    while (true) {
      Chunk.readFrom(input) match {
        case Chunk(NGConstants.CHUNKTYPE_EXIT, code) =>
          return
        case Chunk(NGConstants.CHUNKTYPE_STDOUT, data) =>
          processor.process(Event.from(data.getBytes))
        case Chunk(NGConstants.CHUNKTYPE_STDERR, data) =>
          client.message(Kind.ERROR, data)
        case Chunk(kind, data) =>
          client.message(Kind.ERROR, "Unexpected server output: " + data)
      }
    }
  }
}

private case class Chunk(kind: Chunk.Kind, data: String) {
  def writeTo(output: DataOutputStream) {
    output.writeInt(data.length)
    output.writeByte(kind.toByte)
    output.writeBytes(data)
  }
}

private object Chunk {
  type Kind = Char

  def readFrom(input: DataInputStream): Chunk = {
    val size = input.readInt()
    val kind = input.readByte().toChar
    val data = {
      val buffer = new Array[Byte](size)
      input.readFully(buffer)
      new String(buffer)
    }
    Chunk(kind, data)
  }
}
