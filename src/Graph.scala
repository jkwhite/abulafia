import scala.util.Random


class Graph {
    private var vs = scala.collection.mutable.Map[String,Word]()
    private var es = scala.collection.mutable.Map[String,Link]()


    def add(w:String) : Word = {
        if(!vs.contains(w)) {
            vs+=(w -> new Word(w, false))
        }
        return vs(w)
    }

    def get(w:String) : Word = {
        return vs(w)
    }

    def add(src:Word, dst:Word, distance:Int) : Link = {
        val k = linkKey(src, dst,distance);
        if(!es.contains(k)) {
            es+=(k -> new Link(src,dst,distance))
        }
        val link = es(k)
        link.strengthen()
        return link
    }

    def link(src:String, dst:String, distance:Int) = {
        val sv = add(src)
        val dv = add(dst)
        val link = add(sv,dv,distance)

        sv.addOut(link)
        dv.addIn(link)
    }

    def linkKey(link:Link) : String = {
        return link.src.text+"::::"+link.dst.text
    }

    def linkKey(src:Word, dst:Word, distance:Int) : String = {
        return linkKey(src.text, dst.text, distance)
    }

    def linkKey(src:String, dst:String, distance:Int) : String = {
        return src+"::"+distance+"::"+dst
    }

    def generate : String = {
        val r = new Random()
        val b = new StringBuilder()
        var w = vs("__START__").next(r)
        while(w.text!="__STOP__") {
            b.append(w.text).append(" ")
            w = w.next(r)
        }
        return b.toString
    }

    def generate2(distance:Int, opt:Options) : String = {
        val r = new Random()
        var chosen = new scala.collection.mutable.ArrayBuffer[Word]
        var tries = 0
        do {
            val ws = new scala.collection.mutable.ArrayBuffer[Word]
            ws+=vs("__START__")
            while(ws.last.text!="__STOP__") {
                ws+=ws.last.next(r, ws.takeRight(distance), opt)
            }
            tries = tries + 1
            if(chosen.size < ws.size) chosen = ws
        } while(chosen.size < opt.min && tries < 1000)

        val b = new StringBuilder()
        var last = chosen(0)
        chosen.slice(1, chosen.size-1).foreach(w => {
            if(w.separate(last)) b.append(" ")
            b.append(w.text)
            last = w
        })
        return b.substring(1).toString
    }

    override def toString : String = {
        vs.keySet.toString
    }
}
