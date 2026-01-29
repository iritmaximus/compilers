import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket};
import java.util.stream.Stream



@main def hello(): Unit =
  println("Hello world!")
  println(msg)
  run_server()

def msg = "I was compiled by Scala 3. :)"

def run_server(): Unit =
  println("Starting server...")
  val serverSocket: ServerSocket = new ServerSocket(3000);
  println("Waiting for connections...")

  while true do
    // Accept incoming client connection
    val clientSocket: Socket = serverSocket.accept();
    println("Client connected!");

    // Setup input and output streams for communication with the client
    val in: BufferedReader = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
    val out: PrintWriter = new PrintWriter(clientSocket.getOutputStream(), true);

    // Read message from client
    val message: String = in.readLine();
    println("Client says: " + message);

    val json: ujson.Value = ujson.read(message)
    val command = json("command").str

    val response = command match {
      case "ping" => "{}"
      case "compile" => s"{\"program\": \"${compile(json("code").str)}\"}"
      case _ => s"{\"error\": \"Incorrect json request body\"}"
    }

    // Send response to the client
    out.println(response);

def compile(source: String): String =
  return tokenizer(source).toString()


class TokenLocation(column: Int, line: Int)
class TokenLocationDebug()

class Token(value: String, location: TokenLocation | TokenLocationDebug = TokenLocationDebug())

def tokenizer(source: String): List[Token] =
  return List(Token("test"))
