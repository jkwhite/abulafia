import java.util.Scanner
import java.io.StreamTokenizer
import scala.io.Source
//import scala.collection.mutable.List


object Abulafia extends App {
    val spread = Integer.parseInt(args(0))
    val opt = new Options(
        java.lang.Long.parseLong(args(1)),
        java.lang.Long.parseLong(args(2)),
        java.lang.Long.parseLong(args(3))
    )
    val files = args.drop(4)
    //val file = args(0)
    //val sc = new Scanner(Source.fromFile(file).bufferedReader);
    val g = new Graph()
    val p = new Parser()
    //println("files: "+files.length)
    files.foreach(file => {
        //println("reading "+file)
        p.parse(g, file, spread)
    })
    //println("g: "+g)
    for(i <- 1 to 15) println(g.generate2(spread, opt))
}
