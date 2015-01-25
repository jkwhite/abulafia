import java.io.StreamTokenizer
import scala.io.Source


class Parser {
    def parse(g:Graph, file:String, spread:Int) = {
        val sc = new StreamTokenizer(Source.fromFile(file).bufferedReader);
        sc.ordinaryChar('.')
        //println("pat: "+sc.delimiter())
        val ws = new scala.collection.mutable.ArrayBuffer[String]
        val start = "__START__"
        val stop = "__STOP__"
        val endm = "[\\.\\?!]".r
        ws+=start

        while(/*sc.hasNext()*/ sc.nextToken()!=StreamTokenizer.TT_EOF) {
            //val w = sc.next()
            //printf("%s \n", sc.toString())
            sc.ttype match {
                case StreamTokenizer.TT_EOL => {
                }
                case StreamTokenizer.TT_WORD | StreamTokenizer.TT_NUMBER | _ => {
                    val w = if(sc.sval!=null) sc.sval else Character.toString(sc.ttype.toChar)
                    //println("word: '"+w+"'")
                    ws+=w
                    w match {
                        case "."|"?"|"!" => {
                            ws+=stop
                            for(i <- 0 to ws.size-2) {
                                for(j <- i+1 to Math.min(ws.size-1, i+spread)) {
                                    if(i!=j) {
                                        g.link(ws(i), ws(j), j-i)
                                    }
                                }
                            }
                            ws.clear()
                            ws+=start
                        }
                        case _ => {
                            //println("read '"+w+"'")
                        }
                    }
                }
            }
        }
    }
}
