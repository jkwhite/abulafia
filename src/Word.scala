import scala.util.Random
import scala.collection.mutable.Map


class Word(val text:String, val stop:Boolean) {
    private var outs = Map[Int,Set[Link]]()
    private var ins = Map[Int,Set[Link]]()


    def isOpen = text(0) match { case '('|'[' => true case _ => false }

    def separate(prev:Word) : Boolean = {
        return prev.spaceAfter || spaceBefore(prev)
    }

    def spaceBefore(prev:Word) : Boolean = {
        return !prev.isOpen && (text.length>1 || Character.isLetterOrDigit(text(0)) || '('==text(0))
    }

    def spaceAfter : Boolean = {
        return (text match { case "?"|"."|","|"!"|";"|":" => true case _ => false })
    }

    def weight(choices:Map[Word,Long], distance:Int, opt:Options) = {
        if(outs.contains(distance)) {
            outs(distance).foreach(link =>
                if(choices.contains(link.dst)) {
                    choices+=(link.dst -> (opt.bonding * link.strength * choices(link.dst)))
                }
                else {
                    choices+=(link.dst -> (opt.missing * link.strength))
                }
            )
        }
    }

    def addOut(link:Link) = {
        if(!outs.contains(link.distance)) outs+=(link.distance -> Set[Link]())
        outs(link.distance) += link
    }

    def addIn(link:Link) = {
        if(!ins.contains(link.distance)) ins+=(link.distance -> Set[Link]())
        ins(link.distance) += link
    }

    def next(r:Random) : Word = {
        return random(r,outs(1),true)
    }

    def next(r:Random, chain:Seq[Word], opt:Options) : Word = {
        val choices = scala.collection.mutable.Map[Word,Long]()
        outs(1).foreach(o => choices+=(o.dst -> o.strength))
        for(i <- 0 to chain.size-2) {
            chain(i).weight(choices, chain.size-i, opt)
        }
        //println("choices: "+choices)
        var total = 0L
        choices.foreach(total+=_._2)
        var m = Math.round(total * r.nextDouble())
        //var m = r.nextLong(total)
        for(choice <- choices) {
            m -= choice._2
            if(m<=0) { return choice._1 }
        }
        return null
    }

    def prev(r:Random) : Word = {
        return random(r,ins(1),false)
    }

    private def random(r:Random, choices:Set[Link], fwd:Boolean) : Word = {
        var total = 0L
        choices.foreach(total+=_.strength)
        var m = Math.round(total * r.nextDouble())
        for(choice <- choices) {
            m -= choice.strength
            if(m<=0) { return if(fwd) choice.dst else choice.src }
        }
        return null
    }
}
